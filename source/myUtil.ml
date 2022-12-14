module SSet (T : Set.OrderedType) = struct 
  include Stdlib.Set.Make (T) 

  let add t key = add key t
  let mem t key = mem key t
  let find t ~f = find f t
  let find_opt t ~f = find_opt f t
  let fold t ~init ~f = fold f t init
  let map t ~f = map f t
  let iter t ~f = iter f t
  let is_subset left ~of_ = subset left of_
end

module SMap (T : Map.OrderedType) = struct 
  include Stdlib.Map.Make (T) 

  let add t ~key ~data = add key data t
  let mem t key = mem key t
  let find t k = find k t
  let find_opt t k = find_opt k t
  let fold t ~init ~f = fold f t init
  let map t ~f = map f t
  let iter t ~f = iter f t

end