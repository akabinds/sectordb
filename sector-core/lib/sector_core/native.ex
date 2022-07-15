defmodule SectorCore.Native do
  @moduledoc """
  Functions and definitions from NIF defined functions and types in `native/sector_core`.
  """
  use Rustler, otp_app: :sector_core, crate: :sector_core
end
