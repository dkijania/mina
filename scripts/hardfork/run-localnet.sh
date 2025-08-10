#!/usr/bin/env bash

set -euo pipefail

export MINA_LIBP2P_PASS=
export MINA_PRIVKEY_PASS=
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Source logging library
# shellcheck disable=SC1090
source "$SCRIPT_DIR/logging.sh"
# Note: cmd_exec.sh will be sourced after argument parsing to avoid readonly variable conflicts

# Default configuration values
TX_INTERVAL=${TX_INTERVAL:-30s}           # Interval at which to send transactions
DELAY_MIN=${DELAY_MIN:-20}               # Delay between now and genesis timestamp, in minutes
CONF_SUFFIX=${CONF_SUFFIX:-}             # Allows to use develop ledger when equals to .develop
CUSTOM_CONF=${CUSTOM_CONF:-}             # Custom configuration file path
SLOT_TX_END=${SLOT_TX_END:-}             # Specify slot_tx_end parameter in the config
SLOT_CHAIN_END=${SLOT_CHAIN_END:-}       # Specify slot_chain_end parameter in the config
# Mina executable (exported by cmd_exec.sh as readonly)
# Default value set here, but cmd_exec.sh will make it readonly
: ${MINA_EXE:=mina}
GENESIS_LEDGER_DIR=${GENESIS_LEDGER_DIR:-} # Genesis ledger directory
SLOT=${SLOT:-30}                         # Slot duration (a.k.a. block window duration), seconds

show_usage() {
    cat >&2 << EOF
Creates a quick-epoch-turnaround configuration in localnet/ and launches two Mina nodes

Usage: $0 [OPTIONS]

EXECUTION MODE OPTIONS:
  -m, --mina EXECUTABLE         Mina executable path (default: $MINA_EXE) [native mode]
  --docker-container NAME       Use existing docker container [docker mode]
  --docker-image IMAGE          Use docker image [docker mode]
  --docker-network NETWORK      Docker network to use (optional) [docker mode]

NETWORK OPTIONS:
  -i, --tx-interval INTERVAL    Transaction interval (default: $TX_INTERVAL)
  -d, --delay-min MINUTES       Genesis delay in minutes (default: $DELAY_MIN)
  -s, --slot SECONDS            Slot duration in seconds (default: $SLOT)
  --develop                     Use develop ledger
  -c, --config FILE             Custom config file path
  --slot-tx-end SLOT            Slot tx end parameter
  --slot-chain-end SLOT         Slot chain end parameter
  --genesis-ledger-dir DIR      Genesis ledger directory

EXAMPLES:
  # Native execution (default)
  $0 -m /path/to/mina -s 30
  
  # Docker container execution
  $0 --docker-container mina-node --docker-network mina-net -s 30
  
  # Docker image execution  
  $0 --docker-image minaprotocol/mina:latest -s 30

Consider reading script's code for information on optional arguments
EOF
}

parse_arguments() {
    local keys=()
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage; exit 0 ;;
            -d|--delay-min)
                DELAY_MIN="$2"; shift 2 ;;
            -i|--tx-interval)
                TX_INTERVAL="$2"; shift 2 ;;
            --develop)
                CONF_SUFFIX=".develop"; shift ;;
            -m|--mina)
                # Set environment variable before cmd_exec.sh makes it readonly
                export MINA_EXE="$2"
                export MINA_EXECUTION_MODE="native"
                shift 2 ;;
            --docker-container)
                export MINA_DOCKER_CONTAINER="$2"
                export MINA_EXECUTION_MODE="docker"
                shift 2 ;;
            --docker-image)
                export MINA_DOCKER_IMAGE="$2"
                export MINA_EXECUTION_MODE="docker"
                shift 2 ;;
            --docker-network)
                export MINA_DOCKER_NETWORK="$2"
                shift 2 ;;
            -s|--slot)
                SLOT="$2"; shift 2 ;;
            -c|--config)
                CUSTOM_CONF="$2"; shift 2 ;;
            --slot-chain-end)
                SLOT_CHAIN_END="$2"; shift 2 ;;
            --slot-tx-end)
                SLOT_TX_END="$2"; shift 2 ;;
            --genesis-ledger-dir)
                GENESIS_LEDGER_DIR="$2"; shift 2 ;;
            -*)
                log_error "Unknown option: $1"
                show_usage; exit 1 ;;
            *)
                keys+=("$1"); shift ;;
        esac
    done
    
    # Return unused positional arguments
    printf '%s\n' "${keys[@]}"
}

validate_configuration() {
    if [[ -n "$CONF_SUFFIX" ]] && [[ -n "$CUSTOM_CONF" ]]; then
        log_error "Cannot use both --develop and --config options"
        return 1
    fi

    # Validate docker/native configuration consistency
    if [[ -n "${MINA_DOCKER_CONTAINER:-}" ]] && [[ -n "${MINA_DOCKER_IMAGE:-}" ]]; then
        log_error "Cannot specify both --docker-container and --docker-image"
        return 1
    fi

    # Use polymorphic validation from cmd_exec
    if ! validate_mina_config; then
        return 1
    fi
    
    log_debug "Configuration validated successfully"
    log_debug "Execution mode: ${MINA_EXECUTION_MODE:-native}"
    if [[ "${MINA_EXECUTION_MODE:-native}" == "docker" ]]; then
        log_debug "Docker container: ${MINA_DOCKER_CONTAINER:-none}"
        log_debug "Docker image: ${MINA_DOCKER_IMAGE:-none}"
        log_debug "Docker network: ${MINA_DOCKER_NETWORK:-none}"
    else
        log_debug "Mina executable: ${MINA_EXE:-mina}"
    fi
    
    return 0
}

# Parse command line arguments
# Note: parse_arguments may exit for --help, so call it directly
parse_arguments "$@" >/dev/null

# Source command execution library after argument parsing
# to avoid readonly variable conflicts
# shellcheck disable=SC1090
source "$SCRIPT_DIR/cmd_exec.sh"

validate_configuration

calculate_genesis_timestamp() {
    local delay_min="$1"
    local current_time
    current_time=$(date +%s)
    date -u -d "@$((current_time - current_time % 60 + delay_min * 60))" '+%F %H:%M:%S+00:00'
}

setup_configuration_directory() {
    local conf_dir="$1"
    
    log_env_setup "configuration directory" "$conf_dir"
    
    log_file_op "mkdir" "$conf_dir"
    mkdir -p "$conf_dir"
    
    log_file_op "chmod" "0700 $conf_dir"
    chmod 0700 "$conf_dir"
    
    if [[ ! -f "$conf_dir/bp" ]]; then
        log_info "Generating block producer keypair"
        mina_advanced generate-keypair --privkey-path "$conf_dir/bp"
    fi
    
    log_info "Generating libp2p keypairs"
    mina_libp2p generate-keypair --privkey-path "$conf_dir/libp2p_1"
    mina_libp2p generate-keypair --privkey-path "$conf_dir/libp2p_2"
    
    if [[ -z "$CUSTOM_CONF" ]] && [[ ! -f "$conf_dir/ledger.json" ]]; then
        log_info "Generating test ledger"
        log_cmd "(cd $conf_dir && $SCRIPT_DIR/../prepare-test-ledger.sh -c 100000 -b 1000000 \$(cat bp.pub) > ledger.json)"
        if ! (cd "$conf_dir" && "$SCRIPT_DIR/../prepare-test-ledger.sh" -c 100000 -b 1000000 "$(cat bp.pub)" > ledger.json); then
            log_error "Failed to generate test ledger"
            return 1
        fi
    fi
}

generate_base_config() {
    local conf_dir="$1"
    local genesis_timestamp="$2"
    local slot="$3"
    local slot_tx_end="$4"
    local slot_chain_end="$5"
    
    log_config "generate" "base configuration"
    log_debug "Genesis timestamp: $genesis_timestamp, Slot: ${slot}s"
    log_debug "Slot TX end: ${slot_tx_end:-none}, Slot chain end: ${slot_chain_end:-none}"
    
    local slot_ends=""
    if [[ -n "$slot_tx_end" ]]; then
        slot_ends=".daemon.slot_tx_end = $slot_tx_end | "
        log_config "set" "slot_tx_end = $slot_tx_end"
    fi
    if [[ -n "$slot_chain_end" ]]; then
        slot_ends="$slot_ends .daemon.slot_chain_end = $slot_chain_end | "
        log_config "set" "slot_chain_end = $slot_chain_end"
    fi
    
    local update_config_expr="$slot_ends .genesis.genesis_state_timestamp = \"$genesis_timestamp\""
    
    log_file_op "create" "$conf_dir/base.json"
    log_cmd jq "$update_config_expr"
    jq "$update_config_expr" > "$conf_dir/base.json" << EOF
{
  "genesis": {
    "slots_per_epoch": 48,
    "k": 10,
    "grace_period_slots": 3
  },
  "proof": {
    "work_delay": 1,
    "level": "full",
    "transaction_capacity": { "2_to_the": 2 },
    "block_window_duration_ms": ${slot}000
  }
}
EOF
}

setup_daemon_config() {
    local conf_dir="$1"
    local custom_conf="$2"
    
    if [[ -z "$custom_conf" ]]; then
        log_config "generate" "daemon configuration from ledger"
        log_file_op "read" "$conf_dir/ledger.json"
        log_file_op "create" "$conf_dir/daemon.json"
        { echo '{"ledger": {"accounts": '; cat "$conf_dir/ledger.json"; echo '}}'; } > "$conf_dir/daemon.json"
    else
        log_config "use" "custom daemon configuration: $custom_conf"
        log_file_op "copy" "$custom_conf" "$conf_dir/daemon.json"
        cp "$custom_conf" "$conf_dir/daemon.json"
    fi
}

# Calculate genesis timestamp
calculated_timestamp=$(calculate_genesis_timestamp "$DELAY_MIN")
GENESIS_TIMESTAMP=${GENESIS_TIMESTAMP:-"$calculated_timestamp"}

log_info "Starting localnet setup"
log_info "Genesis timestamp: $GENESIS_TIMESTAMP"

# Setup configuration
CONF_DIR="localnet/config"
setup_configuration_directory "$CONF_DIR"
generate_base_config "$CONF_DIR" "$GENESIS_TIMESTAMP" "$SLOT" "$SLOT_TX_END" "$SLOT_CHAIN_END"
setup_daemon_config "$CONF_DIR" "$CUSTOM_CONF"

prepare_node_arguments() {
    local conf_dir="$1"
    local conf_suffix="$2"
    local genesis_ledger_dir="$3"
    
    # Common arguments for both nodes
    COMMON_ARGS=( --file-log-level Info --log-level Error --seed )
    COMMON_ARGS+=( --config-file "$PWD/$conf_dir/base.json" )
    COMMON_ARGS+=( --config-file "$PWD/$conf_dir/daemon$conf_suffix.json" )
    
    # Node-specific arguments
    NODE_ARGS_1=( --libp2p-keypair "$PWD/$conf_dir/libp2p_1" )
    NODE_ARGS_2=( --libp2p-keypair "$PWD/$conf_dir/libp2p_2" )
    
    if [[ -n "$genesis_ledger_dir" ]]; then
        log_info "Setting up genesis ledger directories"
        rm -rf localnet/genesis_{1,2}
        cp -rf "$genesis_ledger_dir" localnet/genesis_1
        cp -rf "$genesis_ledger_dir" localnet/genesis_2
        NODE_ARGS_1+=( --genesis-ledger-dir "$PWD/localnet/genesis_1" )
        NODE_ARGS_2+=( --genesis-ledger-dir "$PWD/localnet/genesis_2" )
    fi
}

launch_nodes() {
    local conf_dir="$1"
    
    log_info "Cleaning runtime directories"
    log_file_op "delete" "localnet/runtime_1 localnet/runtime_2"
    rm -rf localnet/runtime_1 localnet/runtime_2
    
    log_process_op "start" "block producer node"
    local peer_id
    peer_id=$(cat "$conf_dir/libp2p_2.peerid")
    log_debug "Block producer will connect to peer: /ip4/127.0.0.1/tcp/10312/p2p/$peer_id"
    
    log_process_op "start" "block producer daemon in background"
    local bp_pid
    bp_pid=$(mina_daemon_background "${COMMON_ARGS[@]}" \
        --peer "/ip4/127.0.0.1/tcp/10312/p2p/$(cat "$conf_dir/libp2p_2.peerid")" \
        "${NODE_ARGS_1[@]}" \
        --block-producer-key "$PWD/$conf_dir/bp" \
        --config-directory "$PWD/localnet/runtime_1" \
        --client-port 10301 --external-port 10302 --rest-port 10303)
    
    log_process_op "start" "block producer with ID $bp_pid"
    
    log_process_op "start" "snark worker node"
    peer_id=$(cat "$conf_dir/libp2p_1.peerid")
    log_debug "Snark worker will connect to peer: /ip4/127.0.0.1/tcp/10302/p2p/$peer_id"
    
    log_process_op "start" "snark worker daemon in background"
    local sw_pid
    sw_pid=$(mina_daemon_background "${COMMON_ARGS[@]}" \
        "${NODE_ARGS_2[@]}" \
        --peer "/ip4/127.0.0.1/tcp/10302/p2p/$(cat "$conf_dir/libp2p_1.peerid")" \
        --run-snark-worker "$(cat "$conf_dir/bp.pub")" --work-selection seq \
        --config-directory "$PWD/localnet/runtime_2" \
        --client-port 10311 --external-port 10312 --rest-port 10313)
    
    log_process_op "start" "snark worker with ID $sw_pid"
    
    echo "$bp_pid $sw_pid"
}

wait_for_node_ready() {
    local conf_dir="$1"
    
    log_info "Waiting for nodes to be ready"
    log_timing "wait" "for accounts import to succeed"
    while ! mina_accounts import --privkey-path "$PWD/$conf_dir/bp" --rest-server 10313; do
        log_debug "Waiting for accounts import to succeed..."
        log_timing "sleep" "1 minute"
        sleep 1m
    done
    log_info "Accounts imported successfully"
    
    log_info "Exporting staged ledger"
    log_timing "wait" "for ledger export to succeed"
    while ! mina_ledger export staged-ledger --daemon-port 10311 --output localnet/exported_staged_ledger.json; do
        log_debug "Waiting for ledger export to succeed..."
        log_timing "sleep" "1 minute"
        sleep 1m
    done
    log_file_op "create" "localnet/exported_staged_ledger.json"
    log_info "Staged ledger exported successfully"
}

# Check if process/container is still running
is_process_running() {
    local process_id="$1"
    
    case "${MINA_EXECUTION_MODE:-native}" in
        native)
            # For native mode, process_id is a PID
            kill -0 "$process_id" 2>/dev/null
            ;;
        docker)
            if [[ -n "${MINA_DOCKER_CONTAINER:-}" ]]; then
                # For existing container mode, process_id is the container name
                # Check if container is running
                docker ps --filter "name=$process_id" --filter "status=running" --format "{{.Names}}" | grep -q "^$process_id$"
            else
                # For docker image mode, process_id is a container ID
                # Check if container is running
                docker ps --filter "id=$process_id" --filter "status=running" --format "{{.ID}}" | grep -q "^$process_id"
            fi
            ;;
    esac
}

send_transactions() {
    local conf_dir="$1"
    local sw_pid="$2"
    local tx_interval="$3"
    
    log_info "Starting transaction sender with interval: $tx_interval"
    log_timing "wait" "for process/container $sw_pid to end while sending transactions"
    
    local i=0
    while is_process_running "$sw_pid"; do
        log_debug "Transaction sending loop iteration, checking if process/container $sw_pid is still running"
        
        # Send transactions to random accounts from the ledger
        # shuf's exit code is masked by `true` because we do not expect
        # all of the output to be read
        log_cmd "jq -r '.[].pk' < localnet/exported_staged_ledger.json | shuf"
        if ! jq -r '.[].pk' < localnet/exported_staged_ledger.json | { shuf || true; } | while IFS= read -r acc; do
            if ! is_process_running "$sw_pid"; then
                log_debug "Process/container $sw_pid ended, stopping transaction sending"
                break
            fi
            
            log_debug "Attempting to send payment #$i to $acc"
            if mina_client send-payment \
                --sender "$(cat "$conf_dir/bp.pub")" \
                --receiver "$acc" \
                --amount 0.1 \
                --memo "payment_$i" \
                --rest-server 10313 2>/dev/null; then
                i=$((i+1))
                log_info "Sent transaction #$i to $acc"
            else
                log_debug "Failed to send transaction #$i to $acc"
            fi
            
            log_timing "sleep" "$tx_interval"
            sleep "$tx_interval"
        done; then
            log_debug "Transaction sending loop completed"
        fi
    done
    
    log_process_op "stop" "transaction sender (node process/container $sw_pid ended)"
}

# Launch nodes and handle transactions
log_info "Preparing node arguments and launching localnet"
prepare_node_arguments "$CONF_DIR" "$CONF_SUFFIX" "$GENESIS_LEDGER_DIR"

log_debug "Node launch configuration:"
log_debug "  Config directory: $CONF_DIR"
log_debug "  Config suffix: ${CONF_SUFFIX:-none}"
log_debug "  Genesis ledger dir: ${GENESIS_LEDGER_DIR:-none}"

read -r BP_PID SW_PID < <(launch_nodes "$CONF_DIR")

wait_for_node_ready "$CONF_DIR"
send_transactions "$CONF_DIR" "$SW_PID" "$TX_INTERVAL"

# Wait for all background processes/containers to finish
wait_for_background_processes() {
    local bp_pid="$1"
    local sw_pid="$2"
    
    log_process_op "wait" "for all background nodes to finish"
    
    case "${MINA_EXECUTION_MODE:-native}" in
        native)
            # For native mode, wait for actual PIDs
            wait "$bp_pid" "$sw_pid"
            ;;
        docker)
            # For docker mode, wait for containers to stop
            log_debug "Waiting for docker containers to finish: $bp_pid, $sw_pid"
            
            if [[ -n "${MINA_DOCKER_CONTAINER:-}" ]]; then
                # For existing container mode, containers continue running
                # We just wait for the transaction sender to finish
                log_debug "Using existing container mode - processes run indefinitely"
                while is_process_running "$sw_pid"; do
                    sleep 5
                done
            else
                # For docker image mode, wait for containers to finish
                docker wait "$bp_pid" "$sw_pid" >/dev/null || true
            fi
            ;;
    esac
}

wait_for_background_processes "$BP_PID" "$SW_PID"
