aggr.strain <- function(data,list.aggr,type,apex.n,func,...) {
				data=as.data.table(data)
				switch(type,
					volume=data[,x:=Point_1,
					by=list.aggr],
					area=aggregate(x=list(Point_1=data$Point_1),
					by=list.aggr,
					FUN=func),
					base=aggregate(x=list(MOYANTSEP_S1=data$MOYANTSEP_S1,MOY_ANT_S2=data$MOY_ANT_S2,MOY_LAT_S3=data$MOY_LAT_S3,MOY_POST_S4=data$MOY_POST_S4,MOY_INF_S5=data$MOY_INF_S5,MOY_SEP_S6=data$MOY_SEP_S6,Mean=data$Mean),
					by=list.aggr,
					FUN=func),
					long=aggregate(x=list(BAS_SEP_S1=data$BAS_SEP_S1,MOY_SEP_S2=data$MOY_SEP_S2,APIC_SEP_S3=data$APIC_SEP_S3,BAS_LAT_S4=data$BAS_LAT_S4,MOY_LAT_S5=data$MOY_LAT_S5,APIC_LAT_S6=data$APIC_LAT_S6,Mean=data$Mean),
					by=list.aggr,
					FUN=func),
					apic_2seg=aggregate(x=list(BAS_SEP_S1=data$BAS_SEP_S1,MOY_SEP_S2=data$MOY_SEP_S2,APIC_SEP_S3=data$APIC_SEP_S3,BAS_LAT_S4=data$BAS_LAT_S4,MOY_LAT_S5=data$MOY_LAT_S5,APIC_LAT_S6=data$APIC_LAT_S6,Mean=data$Mean),
					by=list.aggr,
					FUN=func),

					apex=aggregate(x=data[,4:(apex.n+4)],
					by=list.aggr,
					FUN=func)
					)
			}
