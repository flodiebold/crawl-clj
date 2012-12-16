A library for connecting to DCSS Webtiles servers.

    user> (require 'crawl)
    nil
    user> (require 'lamina.core)
    nil
    user> (def g (lamina.core/atom-sink @(crawl/connect-and-watch "crawl.s-z.org" 80 "HangedMan")))
    #'user/g
    user> (get-in @g [:player :inv (get-in @g [:player :equip :weapon])])
    {:tile [232], :base-type 0, :quantity 1, :name "+1,+2 trident (pain)", :plus2 2, :inscription "", :flags 65551, :sub-type 24, :plus 1, :slot 0}
    user> (get-in @g [:player :pos])
    {:x 38, :y 7}
    user> (for [[x col] (:map @g) [y cell] col :when (and (:monster cell) (not (:unseen cell)))] (:monster cell))
    ({:id 72, :name "slime creature", :type 82, :attitude :hostile, :base-type 82, :threat 2}
     {:id 73, :name "brown ugly thing", :type 138, :attitude :hostile, :base-type 138, :threat 2}
     {:id 71, :name "very large slime creature", :type 82, :attitude :hostile, :base-type 82, :threat 2})
