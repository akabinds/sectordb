defmodule SectorCore.Native do
  @moduledoc false

  use Rustler, otp_app: :sector_core, crate: :sector_core
end
