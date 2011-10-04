#Performance Log

Test of in-memory database alternatives. The time for the same request was measured for each database.

Request: http://127.0.0.1:2904/route?from=26999575&to=275283807
##Using ETS
"time":211519
##Using Membase
###mcd library
"time":19172683
###merle library
"time":17069207
##Using Redis
###eredis library
"time":8140534