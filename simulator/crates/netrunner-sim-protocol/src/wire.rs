use std::num::NonZeroU64;

use netrunner_sim_core::{
    ActionDescriptor, ActionTarget, ChanceDescriptor, ChanceOutcome, ExactWeight, PerspectiveCard,
    SemanticAction, Side, TerminalBoundary, TerminalReason, UnsupportedBoundary, UnsupportedCode,
    Zone,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WireSide {
    Corp,
    Runner,
}

impl From<Side> for WireSide {
    fn from(value: Side) -> Self {
        match value {
            Side::Corp => Self::Corp,
            Side::Runner => Self::Runner,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WireZone {
    Deck,
    Hand,
    Discard,
    Installed,
    Scored,
    RemovedFromGame,
    PlayArea,
}

impl From<Zone> for WireZone {
    fn from(value: Zone) -> Self {
        match value {
            Zone::Deck => Self::Deck,
            Zone::Hand => Self::Hand,
            Zone::Discard => Self::Discard,
            Zone::Installed => Self::Installed,
            Zone::Scored => Self::Scored,
            Zone::RemovedFromGame => Self::RemovedFromGame,
            Zone::PlayArea => Self::PlayArea,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "knowledge", rename_all = "snake_case")]
pub enum WirePerspectiveCard {
    Known { instance_id: u32, card_def_id: u32 },
    Opaque { slot: u32, generation: u16 },
}

impl From<PerspectiveCard> for WirePerspectiveCard {
    fn from(value: PerspectiveCard) -> Self {
        match value {
            PerspectiveCard::Known {
                instance_id,
                card_def_id,
            } => Self::Known {
                instance_id: instance_id.0,
                card_def_id: card_def_id.0,
            },
            PerspectiveCard::Opaque(token) => Self::Opaque {
                slot: token.slot.0,
                generation: token.generation,
            },
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "target", rename_all = "snake_case")]
pub enum WireActionTarget {
    Card { card: WirePerspectiveCard },
    Zone { side: WireSide, zone: WireZone },
    Server { server_id: u32 },
    OpaqueSlot { slot: u32, generation: u16 },
}

impl From<ActionTarget> for WireActionTarget {
    fn from(value: ActionTarget) -> Self {
        match value {
            ActionTarget::Card(card) => Self::Card { card: card.into() },
            ActionTarget::Zone { side, zone } => Self::Zone {
                side: side.into(),
                zone: zone.into(),
            },
            ActionTarget::Server(server_id) => Self::Server {
                server_id: server_id.0,
            },
            ActionTarget::OpaqueSlot(token) => Self::OpaqueSlot {
                slot: token.slot.0,
                generation: token.generation,
            },
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", content = "ability_id", rename_all = "snake_case")]
pub enum WireSemanticAction {
    PassPriority,
    EndPhase,
    PlayCard,
    InstallCard,
    UseAbility(u32),
    AdvanceCard,
    ScoreAgenda,
    RezCard,
    InitiateRun,
    ContinueRun,
    ChooseCard,
    ChooseNumber,
    PayCredits,
}

impl From<SemanticAction> for WireSemanticAction {
    fn from(value: SemanticAction) -> Self {
        match value {
            SemanticAction::PassPriority => Self::PassPriority,
            SemanticAction::EndPhase => Self::EndPhase,
            SemanticAction::PlayCard => Self::PlayCard,
            SemanticAction::InstallCard => Self::InstallCard,
            SemanticAction::UseAbility(id) => Self::UseAbility(id.0),
            SemanticAction::AdvanceCard => Self::AdvanceCard,
            SemanticAction::ScoreAgenda => Self::ScoreAgenda,
            SemanticAction::RezCard => Self::RezCard,
            SemanticAction::InitiateRun => Self::InitiateRun,
            SemanticAction::ContinueRun => Self::ContinueRun,
            SemanticAction::ChooseCard => Self::ChooseCard,
            SemanticAction::ChooseNumber => Self::ChooseNumber,
            SemanticAction::PayCredits => Self::PayCredits,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WireActionDescriptor {
    pub id: u32,
    pub actor: WireSide,
    pub timing_window_id: u32,
    pub action: WireSemanticAction,
    pub target: Option<WireActionTarget>,
    pub amount: Option<u16>,
}

impl From<&ActionDescriptor> for WireActionDescriptor {
    fn from(value: &ActionDescriptor) -> Self {
        Self {
            id: value.id.0,
            actor: value.actor.into(),
            timing_window_id: value.timing_window_id.0,
            action: value.action.into(),
            target: value.target.map(Into::into),
            amount: value.amount,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WireExactWeight {
    pub numerator: u64,
    pub denominator: NonZeroU64,
}

impl From<ExactWeight> for WireExactWeight {
    fn from(value: ExactWeight) -> Self {
        Self {
            numerator: value.numerator(),
            denominator: value.denominator_nonzero(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum WireChanceDescriptor {
    Draw {
        side: WireSide,
        card: WirePerspectiveCard,
    },
    RandomAccess {
        server_id: u32,
        card: WirePerspectiveCard,
    },
    ShufflePosition {
        side: WireSide,
        card: WirePerspectiveCard,
        position: u16,
    },
    ResolveOpaqueSlot {
        slot: u32,
        generation: u16,
    },
}

impl From<&ChanceDescriptor> for WireChanceDescriptor {
    fn from(value: &ChanceDescriptor) -> Self {
        match value {
            ChanceDescriptor::Draw { side, card } => Self::Draw {
                side: (*side).into(),
                card: (*card).into(),
            },
            ChanceDescriptor::RandomAccess { server, card } => Self::RandomAccess {
                server_id: server.0,
                card: (*card).into(),
            },
            ChanceDescriptor::ShufflePosition {
                side,
                card,
                position,
            } => Self::ShufflePosition {
                side: (*side).into(),
                card: (*card).into(),
                position: *position,
            },
            ChanceDescriptor::ResolveOpaqueSlot(token) => Self::ResolveOpaqueSlot {
                slot: token.slot.0,
                generation: token.generation,
            },
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WireChanceOutcome {
    pub id: u32,
    pub descriptor: WireChanceDescriptor,
    pub weight: WireExactWeight,
}

impl From<&ChanceOutcome> for WireChanceOutcome {
    fn from(value: &ChanceOutcome) -> Self {
        Self {
            id: value.id.0,
            descriptor: (&value.descriptor).into(),
            weight: value.weight.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WireTerminalReason {
    AgendaPoints,
    Flatline,
    DeckExhaustion,
    Concession,
}

impl From<TerminalReason> for WireTerminalReason {
    fn from(value: TerminalReason) -> Self {
        match value {
            TerminalReason::AgendaPoints => Self::AgendaPoints,
            TerminalReason::Flatline => Self::Flatline,
            TerminalReason::DeckExhaustion => Self::DeckExhaustion,
            TerminalReason::Concession => Self::Concession,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WireTerminal {
    pub winner: WireSide,
    pub reason: WireTerminalReason,
    pub corp_agenda_points: u8,
    pub runner_agenda_points: u8,
}

impl From<&TerminalBoundary> for WireTerminal {
    fn from(value: &TerminalBoundary) -> Self {
        Self {
            winner: value.winner.into(),
            reason: value.reason.into(),
            corp_agenda_points: value.corp_agenda_points,
            runner_agenda_points: value.runner_agenda_points,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WireUnsupportedCode {
    Card,
    Mechanic,
    TimingWindow,
    ChanceModel,
    StateInvariant,
}

impl From<UnsupportedCode> for WireUnsupportedCode {
    fn from(value: UnsupportedCode) -> Self {
        match value {
            UnsupportedCode::Card => Self::Card,
            UnsupportedCode::Mechanic => Self::Mechanic,
            UnsupportedCode::TimingWindow => Self::TimingWindow,
            UnsupportedCode::ChanceModel => Self::ChanceModel,
            UnsupportedCode::StateInvariant => Self::StateInvariant,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WireUnsupported {
    pub timing_window_id: Option<u32>,
    pub code: WireUnsupportedCode,
    pub mechanic: Option<String>,
    pub card: Option<WirePerspectiveCard>,
    pub detail: String,
}

impl From<&UnsupportedBoundary> for WireUnsupported {
    fn from(value: &UnsupportedBoundary) -> Self {
        Self {
            timing_window_id: value.timing_window_id.map(|id| id.0),
            code: value.error.code.into(),
            mechanic: value.error.mechanic.clone(),
            card: value.error.card.map(Into::into),
            detail: value.error.detail.clone(),
        }
    }
}
