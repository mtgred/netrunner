use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{
    TRACE_SCHEMA_VERSION, WireActionDescriptor, WireChanceOutcome, WireSide, WireTerminal,
    WireUnsupported, WireZone,
};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TraceHeader {
    pub schema_version: u16,
    pub core_contract_version: u16,
    pub trace_id: String,
    pub engine: String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RngTelemetry {
    pub algorithm: String,
    pub seed: u64,
    pub draw_index: u64,
    pub sampled_value: u64,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ObservableCard {
    pub card: crate::WirePerspectiveCard,
    pub owner: WireSide,
    pub zone: WireZone,
    pub counters: Vec<(String, i16)>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct NormalizedObservation {
    pub active_side: WireSide,
    pub corp_credits: i16,
    pub corp_clicks: u8,
    pub corp_agenda_points: u8,
    pub runner_credits: i16,
    pub runner_clicks: u8,
    pub runner_agenda_points: u8,
    pub cards: Vec<ObservableCard>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum TraceRecord {
    Header {
        #[serde(flatten)]
        header: TraceHeader,
    },
    Action {
        sequence: u64,
        descriptor: WireActionDescriptor,
    },
    Chance {
        sequence: u64,
        perspective: WireSide,
        outcome: WireChanceOutcome,
        rng: Option<RngTelemetry>,
    },
    Observation {
        sequence: u64,
        perspective: WireSide,
        observable_hash: String,
        state: NormalizedObservation,
    },
    Checkpoint {
        sequence: u64,
        corp_observable_hash: String,
        runner_observable_hash: String,
    },
    Terminal {
        sequence: u64,
        terminal: WireTerminal,
    },
    Unsupported {
        sequence: u64,
        unsupported: WireUnsupported,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SemanticTrace {
    pub header: TraceHeader,
    pub records: Vec<TraceRecord>,
}

impl SemanticTrace {
    pub fn new(trace_id: impl Into<String>, engine: impl Into<String>) -> Self {
        Self {
            header: TraceHeader {
                schema_version: TRACE_SCHEMA_VERSION,
                core_contract_version: netrunner_sim_core::CORE_CONTRACT_VERSION.0,
                trace_id: trace_id.into(),
                engine: engine.into(),
            },
            records: Vec::new(),
        }
    }

    /// Serializes the header and body as one JSON object per line.
    ///
    /// # Errors
    ///
    /// Returns an error for unsupported versions, a header in the body, or JSON
    /// serialization failure.
    pub fn to_jsonl(&self) -> Result<String, TraceError> {
        self.validate_version()?;
        let mut lines = Vec::with_capacity(self.records.len() + 1);
        lines.push(serde_json::to_string(&TraceRecord::Header {
            header: self.header.clone(),
        })?);
        for record in &self.records {
            if matches!(record, TraceRecord::Header { .. }) {
                return Err(TraceError::HeaderInBody);
            }
            lines.push(serde_json::to_string(record)?);
        }
        Ok(lines.join("\n") + "\n")
    }

    /// Parses and validates a complete semantic JSONL trace.
    ///
    /// # Errors
    ///
    /// Returns an error for malformed JSON, a missing or repeated header, or an
    /// unsupported schema/core contract version.
    pub fn from_jsonl(input: &str) -> Result<Self, TraceError> {
        let mut lines = input.lines().filter(|line| !line.trim().is_empty());
        let first = lines.next().ok_or(TraceError::MissingHeader)?;
        let TraceRecord::Header { header } = serde_json::from_str(first)? else {
            return Err(TraceError::MissingHeader);
        };

        let trace = Self {
            header,
            records: lines
                .map(serde_json::from_str)
                .collect::<Result<Vec<_>, _>>()?,
        };
        trace.validate_version()?;
        if trace
            .records
            .iter()
            .any(|record| matches!(record, TraceRecord::Header { .. }))
        {
            return Err(TraceError::HeaderInBody);
        }
        Ok(trace)
    }

    fn validate_version(&self) -> Result<(), TraceError> {
        if self.header.schema_version != TRACE_SCHEMA_VERSION {
            return Err(TraceError::UnsupportedSchemaVersion {
                found: self.header.schema_version,
                supported: TRACE_SCHEMA_VERSION,
            });
        }
        if self.header.core_contract_version != netrunner_sim_core::CORE_CONTRACT_VERSION.0 {
            return Err(TraceError::UnsupportedCoreContractVersion {
                found: self.header.core_contract_version,
                supported: netrunner_sim_core::CORE_CONTRACT_VERSION.0,
            });
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum TraceError {
    Json(serde_json::Error),
    MissingHeader,
    HeaderInBody,
    UnsupportedSchemaVersion { found: u16, supported: u16 },
    UnsupportedCoreContractVersion { found: u16, supported: u16 },
}

impl fmt::Display for TraceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Json(error) => write!(formatter, "invalid trace JSON: {error}"),
            Self::MissingHeader => formatter.write_str("trace must begin with a header record"),
            Self::HeaderInBody => {
                formatter.write_str("trace header may only appear as the first record")
            }
            Self::UnsupportedSchemaVersion { found, supported } => {
                write!(
                    formatter,
                    "unsupported trace schema version {found}; this build supports {supported}"
                )
            }
            Self::UnsupportedCoreContractVersion { found, supported } => {
                write!(
                    formatter,
                    "unsupported core contract version {found}; this build supports {supported}"
                )
            }
        }
    }
}

impl std::error::Error for TraceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Json(error) => Some(error),
            _ => None,
        }
    }
}

impl From<serde_json::Error> for TraceError {
    fn from(value: serde_json::Error) -> Self {
        Self::Json(value)
    }
}

#[cfg(test)]
mod tests {
    use netrunner_sim_core::{
        ActionDescriptor, ActionId, ChanceDescriptor, ChanceOutcome, ChanceOutcomeId, ExactWeight,
        OpaqueCardToken, OpaqueSlotId, SemanticAction, Side, TimingWindowId,
    };

    use super::*;
    use crate::{WireActionDescriptor, WireChanceOutcome, WireTerminalReason};

    fn representative_trace() -> SemanticTrace {
        let action = ActionDescriptor {
            id: ActionId(11),
            actor: Side::Runner,
            timing_window_id: TimingWindowId(4),
            action: SemanticAction::InitiateRun,
            target: None,
            amount: None,
        };
        let outcome = ChanceOutcome {
            id: ChanceOutcomeId(8),
            descriptor: ChanceDescriptor::ResolveOpaqueSlot(OpaqueCardToken {
                slot: OpaqueSlotId(3),
                generation: 1,
            }),
            weight: ExactWeight::new(1, 5).unwrap(),
        };
        let mut trace = SemanticTrace::new("fixture-1", "netrunner-sim");
        trace.records = vec![
            TraceRecord::Action {
                sequence: 1,
                descriptor: WireActionDescriptor::from(&action),
            },
            TraceRecord::Chance {
                sequence: 2,
                perspective: WireSide::Runner,
                outcome: WireChanceOutcome::from(&outcome),
                rng: Some(RngTelemetry {
                    algorithm: "chacha8".to_owned(),
                    seed: 42,
                    draw_index: 9,
                    sampled_value: 2,
                }),
            },
            TraceRecord::Observation {
                sequence: 3,
                perspective: WireSide::Corp,
                observable_hash: "corp-hash".to_owned(),
                state: NormalizedObservation {
                    active_side: WireSide::Runner,
                    corp_credits: 5,
                    corp_clicks: 3,
                    corp_agenda_points: 0,
                    runner_credits: 5,
                    runner_clicks: 4,
                    runner_agenda_points: 0,
                    cards: Vec::new(),
                },
            },
            TraceRecord::Checkpoint {
                sequence: 4,
                corp_observable_hash: "corp-hash".to_owned(),
                runner_observable_hash: "runner-hash".to_owned(),
            },
            TraceRecord::Terminal {
                sequence: 5,
                terminal: WireTerminal {
                    winner: WireSide::Runner,
                    reason: WireTerminalReason::AgendaPoints,
                    corp_agenda_points: 4,
                    runner_agenda_points: 6,
                },
            },
            TraceRecord::Unsupported {
                sequence: 6,
                unsupported: crate::WireUnsupported {
                    timing_window_id: Some(4),
                    code: crate::WireUnsupportedCode::Card,
                    mechanic: None,
                    card: Some(crate::WirePerspectiveCard::Opaque {
                        slot: 12,
                        generation: 1,
                    }),
                    detail: "fixture unsupported card".to_owned(),
                },
            },
        ];
        trace
    }

    #[test]
    fn schema_round_trips_every_record_kind() {
        let trace = representative_trace();
        let jsonl = trace.to_jsonl().unwrap();
        assert_eq!(SemanticTrace::from_jsonl(&jsonl).unwrap(), trace);
        assert_eq!(jsonl.lines().count(), 7);
    }

    #[test]
    fn rejects_future_schema_versions() {
        let input = r#"{"record":"header","schema_version":2,"core_contract_version":1,"trace_id":"future","engine":"test"}"#;
        assert!(matches!(
            SemanticTrace::from_jsonl(input),
            Err(TraceError::UnsupportedSchemaVersion {
                found: 2,
                supported: TRACE_SCHEMA_VERSION
            })
        ));
    }

    #[test]
    fn rejects_future_core_contract_versions() {
        let input = r#"{"record":"header","schema_version":1,"core_contract_version":2,"trace_id":"future","engine":"test"}"#;
        assert!(matches!(
            SemanticTrace::from_jsonl(input),
            Err(TraceError::UnsupportedCoreContractVersion {
                found: 2,
                supported: 1
            })
        ));
    }

    #[test]
    fn rejects_zero_denominator_chance_weights() {
        let input = concat!(
            "{\"record\":\"header\",\"schema_version\":1,\"core_contract_version\":1,",
            "\"trace_id\":\"invalid-weight\",\"engine\":\"test\"}\n",
            "{\"record\":\"chance\",\"sequence\":1,\"perspective\":\"runner\",",
            "\"outcome\":{\"id\":1,\"descriptor\":{\"kind\":\"resolve_opaque_slot\",",
            "\"slot\":1,\"generation\":0},\"weight\":{\"numerator\":1,\"denominator\":0}},",
            "\"rng\":null}\n"
        );
        assert!(matches!(
            SemanticTrace::from_jsonl(input),
            Err(TraceError::Json(_))
        ));
    }
}
