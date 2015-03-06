limitDb <- function(con, sql) {
  if(class(con) == 'PostgreSQLConnection') {
    return(paste(sql, 'limit 1'))
  } else if (class(con) == 'OraConnection') {
    return(paste('select * from (', sql, ') a where ROWNUM = 1'))
  }   
}
