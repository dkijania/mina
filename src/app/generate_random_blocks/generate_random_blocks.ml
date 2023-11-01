open Frontier_base
open Signature_lib
open Mina_base
open Mina_block
open Core
open Async

let () =
  let open Core_kernel in
  Backtrace.elide := false ;
  Async.Scheduler.set_record_backtraces true

let logger = Logger.create ()

let precomputed_values = Lazy.force Precomputed_values.for_unit_tests

let constraint_constants = precomputed_values.constraint_constants

let ledger_depth = constraint_constants.ledger_depth

let proof_level = precomputed_values.proof_level

let verifier =
  Async.Thread_safe.block_on_async_exn (fun () ->
      Verifier.create ~logger ~proof_level ~constraint_constants ~conf_dir:None
        ~pids:(Child_processes.Termination.create_pid_table ()) )

module Genesis_ledger = (val precomputed_values.genesis_ledger)

let accounts_with_secret_keys = Lazy.force Genesis_ledger.accounts

let max_length = 5

let gen_breadcrumb =
  Breadcrumb.For_tests.gen ~logger ~precomputed_values ~verifier
    ?trust_system:None ~accounts_with_secret_keys

let gen_breadcrumb_seq =
  Breadcrumb.For_tests.gen_seq ~logger ~precomputed_values ~verifier
    ?trust_system:None ~accounts_with_secret_keys

module Transfer = Ledger_transfer.Make (Ledger) (Ledger)

let create_frontier () =
  let open Core in
  let open Deferred.Let_syntax in
  let epoch_ledger_location =
    Filename.temp_dir_name ^/ "epoch_ledger"
    ^ (Uuid_unix.create () |> Uuid.to_string)
  in
  let consensus_local_state =
    Consensus.Data.Local_state.create Public_key.Compressed.Set.empty
      ~genesis_ledger:Genesis_ledger.t
      ~genesis_epoch_data:precomputed_values.genesis_epoch_data
      ~epoch_ledger_location ~ledger_depth:constraint_constants.ledger_depth
      ~genesis_state_hash:
        (State_hash.With_state_hashes.state_hash
           precomputed_values.protocol_state_with_hashes )
  in
  let root_ledger =
    Or_error.ok_exn
      (Transfer.transfer_accounts
         ~src:(Lazy.force Genesis_ledger.t)
         ~dest:(Ledger.create ~depth:ledger_depth ()) )
  in
  Protocol_version.(set_current zero) ;
 
  let create_ledger_as_list ledger =
    let%map accounts = Ledger.to_list ledger in
    List.map accounts ~f:(fun acc ->
        Genesis_ledger_helper.Accounts.Single.of_account acc None )

  in
  let%bind genesis_ledger_as_list = create_ledger_as_list root_ledger
  in

  let snarked_ledger_hash =
    root_ledger |> Mina_base.Ledger.merkle_root
    |> Mina_base.Frozen_ledger_hash.of_ledger_hash |> Mina_base.Frozen_ledger_hash.to_decimal_string
  in
  let genesis_ledger : Runtime_config.Ledger.t =
    { base = Accounts genesis_ledger_as_list
    ; num_accounts = Some (Ledger.num_accounts root_ledger)
    ; balances = []
    ; hash = Some snarked_ledger_hash
    ; name = Some "test"
    ; add_genesis_winner = Some true
    }
  in
  Core_kernel.printf !"%{Yojson.Safe}\n"
  (Runtime_config.Ledger.to_yojson genesis_ledger) ;

  let root_data =
    let open Root_data in
    { transition =
        External_transition.Validated.lift @@ Mina_block.Validated.lift
        @@ Mina_block.genesis ~precomputed_values
    ; staged_ledger =
        Staged_ledger.create_exn ~constraint_constants ~ledger:root_ledger
    ; protocol_states = []
    }
  in
  let persistent_root =
    Persistent_root.create ~logger
      ~directory:(Filename.temp_file "snarked_ledger" "")
      ~ledger_depth
  in
  Persistent_root.reset_to_genesis_exn persistent_root ~precomputed_values ;
  let persistent_root_instance =
    Persistent_root.create_instance_exn persistent_root
  in
  Deferred.return (Full_frontier.create ~logger ~root_data
    ~root_ledger:(Ledger.Any_ledger.cast (module Ledger) root_ledger)
    ~consensus_local_state ~max_length ~precomputed_values
    ~time_controller:(Block_time.Controller.basic ~logger)
    ~persistent_root_instance)

let clean_up_persistent_root ~frontier =
  let persistent_root_instance =
    Full_frontier.persistent_root_instance frontier
  in
  Persistent_root.Instance.destroy persistent_root_instance

(* This executable outputs random block to stderr in sexp and json
   The output is useful for src/lib/mina_block tests when the sexp/json representation changes. *)
(* TODO make generation more feauture-rich:
   * include snark works
   * include all types of transactions
   * etc.
*)
let single make_breadcrumb =
  Async.Thread_safe.block_on_async_exn (fun () ->
      let open Deferred.Let_syntax in
      let%bind frontier = create_frontier () in
      let root = Full_frontier.root frontier in
      let open Async_kernel.Deferred.Let_syntax in
      let%map breadcrumb = make_breadcrumb root in
      let block = Breadcrumb.block breadcrumb in
      let scheduled_time =
        Mina_block.(Header.protocol_state @@ header block)
        |> Mina_state.Protocol_state.blockchain_state
        |> Mina_state.Blockchain_state.timestamp
      in
      let precomputed =
        Mina_block.Precomputed.of_block ~scheduled_time
          (Breadcrumb.block breadcrumb)
      in
      Core_kernel.printf !"%{Yojson.Safe}\n"
        (Mina_block.Precomputed.to_yojson precomputed) ;
      clean_up_persistent_root ~frontier )

      let add_breadcrumb frontier breadcrumb =
        let diffs = Full_frontier.calculate_diffs frontier breadcrumb in
        ignore
          ( Full_frontier.apply_diffs frontier diffs ~has_long_catchup_job:false
              ~enable_epoch_ledger_sync:`Disabled
            : [ `New_root_and_diffs_with_mutants of
                Root_identifier.t option * Diff.Full.With_mutant.t list ] )
  
let f make_seq =
          Async.Thread_safe.block_on_async_exn (fun () ->
            let open Deferred.Let_syntax in
            let%bind frontier = create_frontier () in
              let root = Full_frontier.root frontier in
              let open Async_kernel.Deferred.Let_syntax in
              let%map seq = make_seq root in
              List.iter seq ~f:(fun breadcrumb ->
                let block = Breadcrumb.block breadcrumb in
                let scheduled_time =
                  Mina_block.(Header.protocol_state @@ header block)
                  |> Mina_state.Protocol_state.blockchain_state
                  |> Mina_state.Blockchain_state.timestamp
                in
                let precomputed =
                  Mina_block.Precomputed.of_block ~scheduled_time
                    (Breadcrumb.block breadcrumb)
                in
                Core_kernel.printf !"RandomBlock: %{Yojson.Safe}\n"
                  (Mina_block.Precomputed.to_yojson precomputed) ;
                add_breadcrumb frontier breadcrumb ;
              );  
              clean_up_persistent_root ~frontier ) 

      
let () =
  Command.(
    run
      (let open Let_syntax in
      Command.basic ~summary:"Migrate balances table to extended balances table"
        (let%map_open block_count =
           Param.flag "--block-count"
             ~doc:
               "Number of blocks to generate"
             Param.(required int)
         in
         fun () -> 
            if block_count <= 1 then
              Core_kernel.Quickcheck.test gen_breadcrumb ~trials:1 ~f:single
            else 
              Core_kernel.Quickcheck.test (gen_breadcrumb_seq block_count) ~trials:1 ~f 
        )
      )
  )
