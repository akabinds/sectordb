defmodule SectorCore.Native.InterfaceTypes.Table do
  @moduledoc """
  Table interface type.
  """
  @enforce_keys [:identifier]
  defstruct identifier: nil,
            columns: %{}
end

defmodule SectorCore.Native.InterfaceTypes.Column do
  @moduledoc """
  Column interface type.
  """
  @enforce_keys [:identifier]
  defstruct identifier: nil
end
