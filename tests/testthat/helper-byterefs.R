## a byterefs SQLite store over a local source file (after zaro's
## test-mdimrefs.R): v (y = 2, x = 6) int32, chunks (2, 2), zlib or the
## given codec chain; x an affine coordinate. refuse_y adds a coordinate y
## whose codec zaro doesn't know.
make_byterefs <- function(codec = '{"compressor":{"id":"zlib","level":1},"filters":null}',
                          refuse_y = FALSE) {
  z <- function(v) memCompress(writeBin(as.integer(v), raw(), size = 4L,
                                        endian = "little"), "gzip")
  c0 <- z(c(1, 2, 5, 6)); c1 <- z(c(3, 4, 7, 8))
  src <- tempfile(fileext = ".bin")
  writeBin(c(as.raw(1:7), c0, c1), src)

  db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  q <- function(sql, ...) DBI::dbExecute(con, sql, params = if (...length()) list(...))
  q("CREATE TABLE meta(key TEXT PRIMARY KEY, value TEXT)")
  q("INSERT INTO meta VALUES('format_version', 'byterefs/0.1')")
  q("CREATE TABLE sources(source_id INTEGER PRIMARY KEY, source TEXT)")
  q("INSERT INTO sources VALUES(1, ?)", paste0("/old/", basename(src)))
  q("CREATE TABLE remap(target TEXT, prefix TEXT, replacement TEXT)")
  q("INSERT INTO remap VALUES('local', '/old/', ?)", paste0(dirname(src), "/"))
  q('CREATE TABLE arrays(array TEXT PRIMARY KEY, kind TEXT, storage TEXT,
    dims TEXT, shape TEXT, chunks TEXT, dtype TEXT, fill TEXT, codec TEXT, attrs TEXT,
    affine TEXT, "values" BLOB)')
  q("INSERT INTO arrays VALUES('v', 'data', 'referenced', '[\"y\",\"x\"]',
    '[2,6]', '[2,2]', '<i4', '-1', ?, '{\"units\":\"m\"}', NULL, NULL)", codec)
  q("CREATE TABLE placements(array TEXT, source_id INTEGER, source_array TEXT,
    src_offset TEXT, count TEXT, dest TEXT)")
  q("INSERT INTO placements VALUES('v', 1, 'v', '[0,0]', '[2,6]', '[0,0]')")
  q("INSERT INTO arrays VALUES('x', 'coord', 'affine', '[\"x\"]',
    '[6]', '[6]', '<f8', NULL, NULL, NULL, '{\"start\":10,\"step\":0.5}', NULL)")
  q('CREATE TABLE v(source_id INTEGER, dim_0 INTEGER, dim_1 INTEGER,
    present INTEGER, "offset" BIGINT, size BIGINT, info TEXT, data BLOB)')
  q('INSERT INTO v VALUES(1, 0, 0, 1, 7, ?, NULL, NULL), (1, 0, 1, 1, ?, ?, NULL, NULL),
    (1, 0, 2, 0, NULL, NULL, NULL, NULL)', length(c0), 7 + length(c0), length(c1))
  q('CREATE VIEW refs_v AS SELECT dim_0, dim_1, present,
    CASE WHEN "offset" IS NULL THEN NULL ELSE s.source END AS path, "offset", size, info, data
    FROM v JOIN sources s USING(source_id)')
  if (refuse_y) {
    q("INSERT INTO arrays VALUES('y', 'coord', 'referenced', '[\"y\"]',
      '[2]', '[2]', '<f8', NULL, ?, NULL, NULL, NULL)",
      '[{"name":"bytes","configuration":{"endian":"little"}},{"name":"lzma"}]')
  }
  db
}
