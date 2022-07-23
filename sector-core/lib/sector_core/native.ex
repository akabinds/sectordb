defmodule SectorCore.Native do
  @moduledoc false

  use Rustler, otp_app: :sector_core, crate: :sector_core

  # defp error, do: :erlang.nif_error(:nif_not_loaded)
end
