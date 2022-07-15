defmodule SectorCore.Native do
  use Rustler, otp_app: :sector_core, crate: :sector_core
end
