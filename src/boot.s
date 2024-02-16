.feature pc_assignment
.feature labels_without_colons
.feature c_comments

.include "macros.s"

filebuffer = $0200

; -----------------------------------------------------------------------------------------------

.include "main.s"
.include "rrb.s"
.include "irqload.s"
.include "decruncher.s"
.include "iffl.s"

; ----------------------------------------------------------------------------------------------------
