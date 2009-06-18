(define *remedy* (au:create-node "aumu" "KSRM" "KTOS"))
(au:connect-node *remedy* 0 *au:output-node* 0)
(au:update-graph)
(au:open-view *remedy*)
(au:print-graph)

(play-note (now) rem 