fn main() {
    let manifest = netrunner_sim_gateway::beginner_manifest()
        .expect("the checked-in System Gateway manifest must be valid");
    println!(
        "netrunner-sim-server foundation: core contract v{}, trace schema v{}, {} cards",
        netrunner_sim_core::CORE_CONTRACT_VERSION.0,
        netrunner_sim_protocol::TRACE_SCHEMA_VERSION,
        manifest.cards.len()
    );
}
