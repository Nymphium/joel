open Syntax

module Il : sig
  module Joel : sig
    val normalize : term -> term
    module Opts : sig
      include module type of Joel_opts
      module FullStrategy : sig
        include module type of Joel_opts
        val fullopts : term -> term
      end
    end
  end
  module Cps : sig
    val normalize_with_param :
      term -> term -> handlers -> term
    val normalize : term -> term
    module Opts : sig
      include module type of Cps_opts
      module FullStrategy : sig
        include module type of Cps_opts
        val fullopts : term -> term
      end
    end
  end
end
