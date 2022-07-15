defmodule SectorCore.Native.InterfaceTypes.Table do
  @enforce_keys [:identifier]
  defstruct identifier: nil,
            columns: %{}
end

defmodule SectorCore.Native.InterfaceTypes.Column do
  @enforce_keys [:identifier]
  defstruct identifier: nil
end
