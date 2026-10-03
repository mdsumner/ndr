# Dimension-aware broadcasting

The core engine that makes `temperature * land_mask` work when the two
variables have different (but compatible) dimensions. Broadcasting
aligns by dimension name, inserts size-1 dimensions where needed, and
expands arrays to a common shape.
