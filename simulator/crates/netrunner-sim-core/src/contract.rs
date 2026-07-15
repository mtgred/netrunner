use std::fmt;
use std::num::NonZeroU64;

pub const CORE_CONTRACT_VERSION: ContractVersion = ContractVersion(1);

macro_rules! compact_id {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name(pub u32);

        impl From<u32> for $name {
            fn from(value: u32) -> Self {
                Self(value)
            }
        }
    };
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ContractVersion(pub u16);

compact_id!(AbilityId);
compact_id!(ActionId);
compact_id!(CardDefId);
compact_id!(CardInstanceId);
compact_id!(ChanceOutcomeId);
compact_id!(OpaqueSlotId);
compact_id!(ServerId);
compact_id!(TimingWindowId);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Side {
    Corp,
    Runner,
}

impl Side {
    #[must_use]
    pub const fn opponent(self) -> Self {
        match self {
            Self::Corp => Self::Runner,
            Self::Runner => Self::Corp,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Zone {
    Deck,
    Hand,
    Discard,
    Installed,
    Scored,
    RemovedFromGame,
    PlayArea,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Phase {
    Setup,
    CorpTurnBegins,
    CorpAction,
    CorpDiscard,
    RunnerTurnBegins,
    RunnerAction,
    RunnerDiscard,
    Run,
    Encounter,
    Access,
    GameOver,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BoundaryKind {
    Decision,
    Chance,
    Terminal,
    Unsupported,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Visibility {
    Public,
    CorpOnly,
    RunnerOnly,
}

impl Visibility {
    #[must_use]
    pub const fn visible_to(self, perspective: Side) -> bool {
        matches!(self, Self::Public)
            || matches!(
                (self, perspective),
                (Self::CorpOnly, Side::Corp) | (Self::RunnerOnly, Side::Runner)
            )
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct OpaqueCardToken {
    pub slot: OpaqueSlotId,
    pub generation: u16,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PerspectiveCard {
    Known {
        instance_id: CardInstanceId,
        card_def_id: CardDefId,
    },
    Opaque(OpaqueCardToken),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ActionTarget {
    Card(PerspectiveCard),
    Zone { side: Side, zone: Zone },
    Server(ServerId),
    OpaqueSlot(OpaqueCardToken),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SemanticAction {
    PassPriority,
    EndPhase,
    PlayCard,
    InstallCard,
    UseAbility(AbilityId),
    AdvanceCard,
    ScoreAgenda,
    RezCard,
    InitiateRun,
    ContinueRun,
    ChooseCard,
    ChooseNumber,
    PayCredits,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct PassState {
    pub corp_has_passed: bool,
    pub runner_has_passed: bool,
    pub next_to_act: Side,
}

impl PassState {
    #[must_use]
    pub const fn new(first_to_act: Side) -> Self {
        Self {
            corp_has_passed: false,
            runner_has_passed: false,
            next_to_act: first_to_act,
        }
    }

    #[must_use]
    pub const fn both_passed(self) -> bool {
        self.corp_has_passed && self.runner_has_passed
    }

    pub fn record_pass(&mut self, side: Side) {
        match side {
            Side::Corp => self.corp_has_passed = true,
            Side::Runner => self.runner_has_passed = true,
        }
        self.next_to_act = side.opponent();
    }

    pub fn record_action(&mut self, side: Side) {
        self.corp_has_passed = false;
        self.runner_has_passed = false;
        self.next_to_act = side.opponent();
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TimingWindow {
    pub id: TimingWindowId,
    pub phase: Phase,
    pub pass_state: PassState,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ActionDescriptor {
    pub id: ActionId,
    pub actor: Side,
    pub timing_window_id: TimingWindowId,
    pub action: SemanticAction,
    pub target: Option<ActionTarget>,
    pub amount: Option<u16>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ExactWeight {
    numerator: u64,
    denominator: NonZeroU64,
}

impl ExactWeight {
    /// Creates a reduced exact rational weight.
    ///
    /// # Errors
    ///
    /// Returns [`WeightError::ZeroDenominator`] when `denominator` is zero.
    pub fn new(numerator: u64, denominator: u64) -> Result<Self, WeightError> {
        let denominator = NonZeroU64::new(denominator).ok_or(WeightError::ZeroDenominator)?;
        let divisor = gcd(numerator, denominator.get());
        let reduced_denominator =
            NonZeroU64::new(denominator.get() / divisor).ok_or(WeightError::ZeroDenominator)?;
        Ok(Self {
            numerator: numerator / divisor,
            denominator: reduced_denominator,
        })
    }

    #[must_use]
    pub const fn numerator(self) -> u64 {
        self.numerator
    }

    #[must_use]
    pub const fn denominator(self) -> u64 {
        self.denominator.get()
    }

    #[must_use]
    pub const fn denominator_nonzero(self) -> NonZeroU64 {
        self.denominator
    }
}

impl Default for ExactWeight {
    fn default() -> Self {
        Self {
            numerator: 1,
            denominator: NonZeroU64::MIN,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WeightError {
    ZeroDenominator,
}

impl fmt::Display for WeightError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ZeroDenominator => {
                formatter.write_str("chance weight denominator cannot be zero")
            }
        }
    }
}

impl std::error::Error for WeightError {}

const fn gcd(mut left: u64, mut right: u64) -> u64 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    if left == 0 { 1 } else { left }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ChanceDescriptor {
    Draw {
        side: Side,
        card: PerspectiveCard,
    },
    RandomAccess {
        server: ServerId,
        card: PerspectiveCard,
    },
    ShufflePosition {
        side: Side,
        card: PerspectiveCard,
        position: u16,
    },
    ResolveOpaqueSlot(OpaqueCardToken),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ChanceOutcome {
    pub id: ChanceOutcomeId,
    pub descriptor: ChanceDescriptor,
    pub weight: ExactWeight,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LatentMultisetEntry {
    pub token: OpaqueCardToken,
    pub multiplicity: u16,
    pub weight_per_copy: ExactWeight,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LazyLatentMultiset {
    pub perspective: Side,
    pub owner: Side,
    pub source_zone: Zone,
    pub total_cards: u16,
    pub entries: Vec<LatentMultisetEntry>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ChanceSpace {
    Enumerated(Vec<ChanceOutcome>),
    LazyLatentMultiset(LazyLatentMultiset),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DecisionBoundary {
    pub contract_version: ContractVersion,
    pub perspective: Side,
    pub actor: Side,
    pub timing: TimingWindow,
    pub actions: Vec<ActionDescriptor>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ChanceBoundary {
    pub contract_version: ContractVersion,
    pub perspective: Side,
    pub timing_window_id: TimingWindowId,
    pub space: ChanceSpace,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TerminalReason {
    AgendaPoints,
    Flatline,
    DeckExhaustion,
    Concession,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TerminalBoundary {
    pub contract_version: ContractVersion,
    pub winner: Side,
    pub reason: TerminalReason,
    pub corp_agenda_points: u8,
    pub runner_agenda_points: u8,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnsupportedCode {
    Card,
    Mechanic,
    TimingWindow,
    ChanceModel,
    StateInvariant,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct UnsupportedError {
    pub code: UnsupportedCode,
    pub mechanic: Option<String>,
    pub card: Option<PerspectiveCard>,
    pub detail: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct UnsupportedBoundary {
    pub contract_version: ContractVersion,
    pub timing_window_id: Option<TimingWindowId>,
    pub error: UnsupportedError,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Boundary {
    Decision(DecisionBoundary),
    Chance(ChanceBoundary),
    Terminal(TerminalBoundary),
    Unsupported(UnsupportedBoundary),
}

impl Boundary {
    #[must_use]
    pub const fn kind(&self) -> BoundaryKind {
        match self {
            Self::Decision(_) => BoundaryKind::Decision,
            Self::Chance(_) => BoundaryKind::Chance,
            Self::Terminal(_) => BoundaryKind::Terminal,
            Self::Unsupported(_) => BoundaryKind::Unsupported,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StateContract {
    pub contract_version: ContractVersion,
    pub phase: Phase,
    pub active_side: Side,
    pub timing: TimingWindow,
    pub boundary: Boundary,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rational_weights_are_exact_and_reduced() {
        let weight = ExactWeight::new(12, 18).unwrap();
        assert_eq!(weight.numerator(), 2);
        assert_eq!(weight.denominator(), 3);
        assert_eq!(ExactWeight::new(1, 0), Err(WeightError::ZeroDenominator));
    }

    #[test]
    fn pass_state_preserves_sequential_priority() {
        let mut passes = PassState::new(Side::Corp);
        passes.record_pass(Side::Corp);
        assert!(!passes.both_passed());
        assert_eq!(passes.next_to_act, Side::Runner);
        passes.record_action(Side::Runner);
        assert_eq!(passes, PassState::new(Side::Corp));
    }

    #[test]
    fn opaque_cards_do_not_require_definition_ids() {
        let card = PerspectiveCard::Opaque(OpaqueCardToken {
            slot: OpaqueSlotId(7),
            generation: 2,
        });
        assert!(matches!(card, PerspectiveCard::Opaque(_)));
    }
}
