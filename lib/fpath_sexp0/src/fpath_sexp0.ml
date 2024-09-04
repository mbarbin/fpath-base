module Fpath = struct
  include Fpath
  include Fpath0
  include Path.Export
end

module Absolute_path = Path.Absolute_path
module Relative_path = Path.Relative_path
module Fpart = Fpart
