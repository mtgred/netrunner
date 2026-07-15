use std::sync::Arc;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SyntheticStateSize {
    Opening,
    Midgame,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct StateScalars {
    pub turn: u16,
    pub corp_credits: i16,
    pub runner_credits: i16,
    pub corp_clicks: u8,
    pub runner_clicks: u8,
    pub active_side: u8,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct CardSlot {
    pub instance_id: u32,
    pub card_def_id: u32,
    pub counters: u16,
    pub zone: u8,
    pub flags: u8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OwnedFlatState {
    pub scalars: StateScalars,
    pub cards: Vec<CardSlot>,
    pub server_roots: Vec<u16>,
    pub latent_counts: Vec<u16>,
}

impl OwnedFlatState {
    #[must_use]
    pub fn synthetic(size: SyntheticStateSize) -> Self {
        let (cards, servers, latent) = dimensions(size);
        Self {
            scalars: StateScalars {
                corp_credits: 5,
                runner_credits: 5,
                corp_clicks: 3,
                runner_clicks: 4,
                ..StateScalars::default()
            },
            cards: synthetic_cards(cards),
            server_roots: (0..servers).collect(),
            latent_counts: vec![1; latent],
        }
    }

    pub fn apply_like_mutation(&mut self, index: usize) {
        self.scalars.turn = self.scalars.turn.wrapping_add(1);
        self.scalars.corp_credits = self.scalars.corp_credits.saturating_add(1);
        let card_index = index % self.cards.len();
        let card = &mut self.cards[card_index];
        card.counters = card.counters.wrapping_add(1);
        card.flags ^= 1;
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CowPagedState {
    pub scalars: StateScalars,
    pub cards: Arc<Vec<CardSlot>>,
    pub server_roots: Arc<Vec<u16>>,
    pub latent_counts: Arc<Vec<u16>>,
}

impl CowPagedState {
    #[must_use]
    pub fn synthetic(size: SyntheticStateSize) -> Self {
        let owned = OwnedFlatState::synthetic(size);
        Self {
            scalars: owned.scalars,
            cards: Arc::new(owned.cards),
            server_roots: Arc::new(owned.server_roots),
            latent_counts: Arc::new(owned.latent_counts),
        }
    }

    pub fn apply_like_mutation(&mut self, index: usize) {
        self.scalars.turn = self.scalars.turn.wrapping_add(1);
        self.scalars.corp_credits = self.scalars.corp_credits.saturating_add(1);
        let cards = Arc::make_mut(&mut self.cards);
        let card_index = index % cards.len();
        let card = &mut cards[card_index];
        card.counters = card.counters.wrapping_add(1);
        card.flags ^= 1;
    }
}

const fn dimensions(size: SyntheticStateSize) -> (u32, u16, usize) {
    match size {
        SyntheticStateSize::Opening => (94, 3, 24),
        SyntheticStateSize::Midgame => (150, 8, 48),
    }
}

fn synthetic_cards(count: u32) -> Vec<CardSlot> {
    (0..count)
        .map(|index| CardSlot {
            instance_id: index,
            card_def_id: 30_000 + index % 40,
            counters: u16::try_from(index % 4).expect("counter fixture fits in u16"),
            zone: u8::try_from(index % 7).expect("zone fixture fits in u8"),
            flags: (index % 3 == 0).into(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cow_forks_share_pages_until_mutated() {
        let original = CowPagedState::synthetic(SyntheticStateSize::Midgame);
        let mut fork = original.clone();
        assert!(Arc::ptr_eq(&original.cards, &fork.cards));
        fork.apply_like_mutation(4);
        assert!(!Arc::ptr_eq(&original.cards, &fork.cards));
        assert_ne!(original, fork);
    }

    #[test]
    fn layouts_apply_equivalent_mutations() {
        let mut owned = OwnedFlatState::synthetic(SyntheticStateSize::Opening);
        let mut cow = CowPagedState::synthetic(SyntheticStateSize::Opening);
        for index in 0..8 {
            owned.apply_like_mutation(index);
            cow.apply_like_mutation(index);
        }
        assert_eq!(owned.scalars, cow.scalars);
        assert_eq!(owned.cards, *cow.cards);
    }
}
