defmodule SectorCore.MixProject do
  use Mix.Project

  def project do
    [
      app: :sector_core,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:rustler, "~> 0.25.0"},
      {:credo, "~> 1.6", only: [:dev, :test], runtime: false}
    ]
  end

  defp aliases do
    [
      fmtall: [
        "format",
        "cmd cargo fmt --manifest-path native/sector_core/Cargo.toml"
      ],
      testall: [
        "test",
        "cmd cargo test --manifest-path native/sector_core/Cargo.toml --verbose"
      ],
      analyze: [
        "credo",
        "cmd cargo check --manifest-path native/sector_core/Cargo.toml --verbose --all-targets --profile=test",
        "cmd cargo clippy --manifest-path native/sector_core/Cargo.toml --all-features -- -D warnings"
      ],
      cb: [
        "compile",
        "cmd cargo build --manifest-path native/sector_core/Cargo.toml --verbose"
      ]
    ]
  end
end
