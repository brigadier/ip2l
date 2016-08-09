-record(ip2l, {
	country_short = undefined,
	country_long = undefined,
	region = undefined,
	city = undefined,
	isp = undefined,
	latitude = undefined,
	longitude = undefined,
	domain = undefined,
	zipcode = undefined,
	timezone = undefined,
	netspeed = undefined,
	idd_code = undefined,
	area_code = undefined,
	weather_code = undefined,
	weather_name = undefined,
	mcc = undefined,
	mnc = undefined,
	mobile_brand = undefined,
	elevation = undefined,
	usage_type = undefined
}).

%%-define(BINARY_COPY(Val), Val)
-define(BINARY_COPY(Val), binary:copy(Val)). %% bit slower, more memory, but allows garbage collecting

-define(DEFAULT_WORKERS_NUM, 8).