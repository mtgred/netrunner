//! Checked-in card data required by the first supported System Gateway matchup.

use serde::Deserialize;

pub const BEGINNER_MANIFEST_JSON: &str = include_str!("../data/system-gateway-beginner.json");

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct BeginnerManifest {
    pub schema_version: u16,
    pub agenda_point_target: u8,
    pub corp: BeginnerDeck,
    pub runner: BeginnerDeck,
    pub cards: Vec<GatewayCard>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct BeginnerDeck {
    pub name: String,
    pub side: String,
    pub identity: ManifestCardRef,
    pub cards: Vec<DeckEntry>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct ManifestCardRef {
    pub card_def_id: u32,
    pub title: String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct DeckEntry {
    pub quantity: u8,
    pub card_def_id: u32,
    pub title: String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct GatewayCard {
    pub card_def_id: u32,
    pub title: String,
    pub side: String,
    pub card_type: String,
}

/// Parses the checked-in beginner manifest.
///
/// # Errors
///
/// Returns an error if the checked-in JSON does not match the manifest schema.
pub fn beginner_manifest() -> Result<BeginnerManifest, serde_json::Error> {
    serde_json::from_str(BEGINNER_MANIFEST_JSON)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn checked_in_manifest_has_exact_beginner_pool() {
        let manifest = beginner_manifest().unwrap();
        assert_eq!(manifest.schema_version, 1);
        assert_eq!(manifest.agenda_point_target, 6);
        assert_eq!(
            manifest.corp.identity.title,
            "The Syndicate: Profit over Principle"
        );
        assert_eq!(
            manifest.runner.identity.title,
            "The Catalyst: Convention Breaker"
        );
        assert_eq!(
            manifest
                .corp
                .cards
                .iter()
                .map(|entry| usize::from(entry.quantity))
                .sum::<usize>(),
            34
        );
        assert_eq!(
            manifest
                .runner
                .cards
                .iter()
                .map(|entry| usize::from(entry.quantity))
                .sum::<usize>(),
            30
        );
        let unique_deck_cards = manifest
            .corp
            .cards
            .iter()
            .chain(&manifest.runner.cards)
            .map(|entry| entry.card_def_id)
            .collect::<HashSet<_>>();
        assert_eq!(unique_deck_cards.len(), 32);
        assert_eq!(manifest.cards.len(), 34);
        assert!(manifest.cards.iter().any(|card| card.title == "Brân 1.0"));
        assert!(manifest.cards.iter().any(|card| card.title == "Karunā"));
    }
}
