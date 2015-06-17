.model vme_read_write
.inputs dsr dsw ldtack
.outputs d dtack lds
.graph
p1 dsr+ dsw+
p2 lds+ lds+/1

dsr+ lds+
lds+ ldtack+
ldtack+ d+
d+ dtack+
dtack+ dsr-
dsr- d-
d- p3 p4
p3 lds-
lds- ldtack-
ldtack- p2

dsw+ d+/1
d+/1 lds+/1
lds+/1 ldtack+/1
ldtack+/1 d-/1
d-/1 p3 dtack+/1
dtack+/1 dsw-
dsw- p4
p4 dtack-
dtack- p1
.marking {p1 p2}
.end

