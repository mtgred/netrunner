#!/bin/bash
# ---------------------------
# Author: Brandon Patram
# Date: 2018-06-19
#
# Description: List out merge commits and one off commits between
# the last tagged release and the current state of master.
#
# Usage: publish-release.sh [-y] [-h] [-v] [-V]
# Examples:
# ./scripts/publish-release.sh -y
# ./scripts/publish-release.sh -y -v
# ---------------------------

function echo_error()     { echo -e "\\033[0;31mERROR: $*\\033[0m"; }
function echo_warn()      { echo -e "\\033[0;33mWARN: $*\\033[0m"; }
function echo_soft_warn() { echo -e "\\033[0;33m$*\\033[0m"; }
function echo_success()   { echo -e "\\033[0;32m$*\\033[0m"; }
function echo_info()      { echo -e "$*\\033[0m"; }


function verify_command_dependency() {
  command -v "$1" >/dev/null 2>&1 || { echo_error >&2 "Missing $1 command. To install run: $2"; return 1; }
}

VERSION="v0.4.0 (2019-08-28)" # internal version of release script

RELEASE_GH_USER=mtgred # upstream repo org or user owner
RELEASE_GH_REPO=netrunner # upstream repo name
RELEASE_BRANCH=master

INTERACTIVE_MODE=true
VERBOSE=false
TAG_PREFIX="v"
BLACKLIST_AUTHORS=(
  "dependabot"
  "dependabot-preview"
  "dependabot-preview[bot]"
)

while getopts "yhvV" option; do
  case "${option}"
    in
      y) INTERACTIVE_MODE=false ;;
      v) VERBOSE=true ;;
      h) echo_info "Usage: $0 [-y] [-h] [-v] [-V]" && exit 0 ;;
      V) echo_info "$VERSION" && exit 0 ;;
      *) echo_warn "Ignoring unknown option $OPTARG" ;;
  esac
done



#######################
# ----- SYSTEM CHECK
#######################



verify_command_dependency "hub" "brew install hub" || exit 1
verify_command_dependency "jq" "brew install jq" || exit 1



#######################
# ----- INITIAL SETUP
#######################



if [ $VERBOSE == true ]; then
  set -x
fi

if ! GH_SESSION_USER=$(hub api user | jq -r .login); then
  echo_error "Unable to authenticate to GitHub API via \`hub\`"
  exit 1
fi

echo_success "Logged in as GitHub user: $GH_SESSION_USER"



#######################
# ----- FIND RELEASE NAME
#######################



read -r -d '' LAST_TAG_GQL << GRAPHQL
query {
	repository(owner: "$RELEASE_GH_USER", name: "$RELEASE_GH_REPO") {
    refs(first: 1, refPrefix: "refs/tags/", orderBy: { field: TAG_COMMIT_DATE, direction: DESC }) {
      edges {
        node {
          tagName: name
          target {
            gitSha: oid
          }
        }
      }
    }
  }
}
GRAPHQL

if ! LAST_TAG_GQL_RESULT=$(hub api graphql -F query="$LAST_TAG_GQL"); then
  echo_error "Fetching last release tag name failed"
  exit 1
fi

LAST_TAG_GQL_COMMIT=$(echo "$LAST_TAG_GQL_RESULT" | jq -r '.data.repository.refs.edges[0].node.target.gitSha')
LAST_TAG_GQL_NAME=$(echo "$LAST_TAG_GQL_RESULT" | jq -r '.data.repository.refs.edges[0].node.tagName')

NEXT_TAG="$((LAST_TAG_GQL_NAME + 1))"

#######################
# ----- BUILD RELEASE NOTES
#######################



echo_info "Querying history since $LAST_TAG_GQL_NAME..."
# get commits after the last tagged commit
# we make an assumption that a commit will only be related to a single pr
# TODO: handle pagination if there are over 100 commits since last release
read -r -d '' HISTORY_GQL << GRAPHQL
query {
	repository(owner: "$RELEASE_GH_USER", name: "$RELEASE_GH_REPO") {
    ref(qualifiedName: "refs/heads/$RELEASE_BRANCH") {
      target {
        ... on Commit {
          history(first: 100, before: "$LAST_TAG_GQL_COMMIT 0") {
            commits: nodes {
              hash: abbreviatedOid
              fullHash: oid
              messageHeadline
              author {
                user {
                  login
                }
              }
              parents {
                totalCount
              }
              prs: associatedPullRequests(first: 1) {
                nodes {
                  merged
                  prTitle: title
                  prNumber: number
                  mergedBy {
                    login
                  }
                  author {
                    login
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
GRAPHQL

if ! HISTORY_GQL_RESULT=$(hub api graphql -F query="$HISTORY_GQL"); then
  echo_error "Fetching unreleased commits and pull requests failed"
  exit 1
fi

echo_info "Preparing release notes..."

# lets break down the logic here for those not familiar with jq
# 1) dig into the graphql result and grab the array of commits
# 2) throw out every commit made on or before the tagged commit (since those are already released)
# 3) normalize the data set to something a bit more readable and consise.
#    in this step we will also omit any blacklisted authors from the list
# 4) lets throw away any non-loose commits associated to the same PR (unique_by)
# 5) transform the normalized data set into a list of strings representing each PR or commit.
#    in this step we will also omit any merge commits, as those are handled by the associated PRs
# 6) join the array into one big string with a line break
read -r -d '' JQ_LOGIC << JQ
  .data.repository.ref.target.history.commits
    | [limit((\$lastTagIndex | tonumber); .[])]
    | map({
        isPr: ((.prs.nodes | length) > 0),
        isMergeCommit: ((.parents.totalCount | tonumber) > 1),
        prId: "#\\(.prs.nodes[0].prNumber)",
        hash: .hash,
        prTitle: .prs.nodes[0].prTitle,
        message: .messageHeadline,
        authors: (
          [
            .prs.nodes[0].author.login,
            .prs.nodes[0].mergedBy.login,
            .author.user.login
          ]
            | map(strings)
            | unique
            | . - (\$blacklistedUsers | split(" "))
            | map("@\\(.)")
            | join(", ")
        )
      })
    | unique_by(
      if .isPr then
        .prId
      else
        .hash
      end
    )
    | map(
      if (.isPr == true) then
        "- \\(.prId): \\(.prTitle) (\\(.authors))"
      elif (.isMergeCommit == false) then
        "- \\(.hash): \\(.message) (\\(.authors))"
      else
        empty
      end
    )
    | join("\\n")
JQ

# github's api returns git history beyond what we need, so we can omit every commit made before
# the last released tag. we can do this by finding the last tag in the commit array and
# then trucate/limit the list to omit those older commits.
# NOTE: we are limited to fetching 100 commits per page, so its possible over 100 changes have been made
# since the last release. doing squash merges should help reduce this likelyhood
if ! LAST_TAG_INDEX=$(echo "$HISTORY_GQL_RESULT" | jq -r \
  --arg lastTagHash "$LAST_TAG_GQL_COMMIT" \
  '.data.repository.ref.target.history.commits | map(.fullHash) | index($lastTagHash)')
then
  echo_error "Could not find last release, maybe there is over 100 changes since last release?"
  exit 1
fi

if [ "$LAST_TAG_INDEX" -eq 0 ]; then
  echo_soft_warn "No changes to release."
  echo_success "Done!"
  exit 0
fi

if ! RELEASE_NOTES=$(echo "$HISTORY_GQL_RESULT" | jq -r \
  --arg lastTagIndex "$LAST_TAG_INDEX" \
  --arg blacklistedUsers "${BLACKLIST_AUTHORS[*]}" \
  "$JQ_LOGIC")
then
  echo_error "Parsing unreleased commits and pull requests failed"
  exit 1
fi



#######################
# ----- MAKE RELEASE
#######################



AS_DRAFT=true
if [ $INTERACTIVE_MODE == false ]; then
  AS_DRAFT=false
  echo_info "Publishing release to GitHub..."
else
  echo_info "Creating draft release to GitHub..."
fi

echo_info "Release notes for $NEXT_TAG:\\n$RELEASE_NOTES"

# TODO: migrate to v4 graphql api call when creating releases is supported
if ! RELEASE_GH_RESULT=$(hub api \
  "repos/$RELEASE_GH_USER/$RELEASE_GH_REPO/releases" \
  --raw-field "tag_name=$NEXT_TAG" \
  --raw-field "name=$NEXT_TAG" \
  --field "target_commitish=$RELEASE_BRANCH" \
  --field "body=$RELEASE_NOTES" \
  --field "draft=$AS_DRAFT" \
  --field "prerelease=false")
then
  echo_error "Failed to create GitHub release\n$(echo "$RELEASE_GH_RESULT" | jq -r '.message')"
  exit 1
fi

if ! RELEASE_URL=$(echo "$RELEASE_GH_RESULT" | jq -r '.html_url'); then
  echo_error "An error occurred after making the release. Please verify release has been made successfully on GitHub."
fi

if [ $AS_DRAFT == true ]; then
  echo_soft_warn "Release not published yet, visit the release URL to finalize publishing: $RELEASE_URL"
else
  echo_success "Published $NEXT_TAG! $RELEASE_URL"
fi
