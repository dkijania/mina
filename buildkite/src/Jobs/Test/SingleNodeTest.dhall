let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let PipelineTag = ../../Pipeline/Tag.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall


let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let DebianVersions = ../../Constants/DebianVersions.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

let buildTestCmd : Size -> Command.Type = \(cmd_target : Size) ->
  let deb_version = DebianVersions.DebVersion.Bullseye
  let key = "single-node-tests" in
  Command.build
    Command.Config::{
      commands = RunInToolchain.runInToolchain ["DUNE_INSTRUMENT_WITH=bisect_ppx", "COVERALLS_TOKEN"] "artifact-cache-helper.sh command_line_tests.exe && chmod +x command_line_tests.exe && buildkite/scripts/single-node-tests.sh && buildkite/scripts/upload-partial-coverage-data.sh ${key}",
      label = "single-node-tests",
      key = key,
      target = cmd_target,
      docker = None Docker.Type,
      depends_on = DebianVersions.dependsOn deb_version
    }

in

Pipeline.build
  Pipeline.Config::{
    spec = 
      let unitDirtyWhen = [
        S.strictlyStart (S.contains "src/lib"),
        S.strictlyStart (S.contains "src/test"),
        S.strictly (S.contains "Makefile"),
        S.exactly "buildkite/src/Jobs/Test/SingleNodeTest" "dhall",
        S.exactly "buildkite/scripts/single-node-tests" "sh"
      ]

      in

      JobSpec::{
        dirtyWhen = unitDirtyWhen,
        path = "Test",
        name = "SingleNodeTest",
        tags = [ PipelineTag.Type.Long, PipelineTag.Type.Test ]
      },
    steps = [
      buildTestCmd Size.XLarge
    ]
  }