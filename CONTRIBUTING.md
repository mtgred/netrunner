# Contributing 

All contributions to the project (bug/feature reports, pull requests) are very welcome and appreciated! To make sure your work will be as useful as possible, make sure you follow these guidelines.

## AI/LLM stance

This is important so it must go up front.

Jinteki.net is built by humans for humans. We are not interested in drive-by PRs, thousand-word issues, or slop of any kind. If you are willing and interested to put in the work to learn the code base, to speak human to human with others, to share and be shared with, we welcome you. If you only wish to pad your stats, or to sit back while a program writes your issues, your comments, your code, or your slack messages, then we are not interested in your contributions.

As a dev team, we understand that it is increasingly hard to avoid using these tools and don't outright reject them, but we have no interest in accepting purely llm-based works from anyone, new dev or old. We volunteer our time and our energy to jnet because we enjoy it, because the work is the work. If you wish to use an llm to help answer a question or to craft a change, then we ask that you limit it to what is necessary, not what is possible.

To be clear, if you are a new contributor and use these tools to generate your issues or PRs, then we will close and/or reject your contributions. If you do not put in the effort to write it, then we will not put in the effort to read and understand it.

## Issues - Bug Reports & Feature Requests

- Before submitting an issue, search existing ones to see if there isn't one already open, so you aren't creating duplicates. If there is already an open issue for your case, simply comment on it to add your voice to the discussion.
- Make sure to include closed issues in your search. Although you may want to open a new one regardless, linking the old discussions (using GitHub hashtag syntax, e.g. #1025) can be very helpful.
- Give your issue a clear and descriptive title. Good title examples: *Parasite trashes ice too early if the ice has additional strength (e.g. Meru Mati)*, *Trashing ice during a run should update the run position*. Bad title examples: ~~nice feature to have~~, ~~Gabriel bug~~, ~~rezzing doesn't work~~, ~~a few suggestions~~
- Following on the last example from the previous point - limit your reports to one bug/feature request per issue. If you have more ideas, just create more issues. Dealing with issues called *some bugs I found* or *a few suggestions for the site* isn't pleasant and lessens the likelihood of your voice to be heard.
- If your issue is connected to a specific card's behavior, check out the card's notes in the [card implementation status sheet](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml). The problem may be already known and noted or the card may be unimplemented at all.
- When reporting a bug, clear reproduction steps, screenshots and a description of your environment (used browser, operating system, etc.) all help a ton with bugfixing. Please provide them if possible!

## Pull Requests - Code & Art Contributions

- Do not add your IDE's project files to project's `gitignore` - add them to your local `.gitignore` instead. See [this section](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development#gitignore) for more info.
- The Clojure files in this project try to follow the [Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide), albeit loosely. Try to make sure your code complies with the guidelines.
- A clear title and description of the changes (plus screenshots, if applicable) in Pull Request go a long way toward speeding up the PR's review and acceptance process.
- To learn how to close GitHub issues with PR's or commit's messages, read [this guide on GitHub](https://help.github.com/articles/closing-issues-via-commit-messages/).
- If you are just starting out, check out our [development guide](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development) - you may find a few things that are useful there.
