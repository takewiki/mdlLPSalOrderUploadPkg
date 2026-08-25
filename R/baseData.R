#' 基础表更新
#'
#' @param dms_token 第二个参数
#' @param erp_token
#' @param FStartDate
#' @param FEndDate
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' baseData_sync
baseData_sync <- function(dms_token,erp_token,FStartDate,FEndDate) {
  print(paste0("表t_customer_customers同步开始，开始时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  DF_token = dms_token
  LP_token = erp_token
  FStartDate=FStartDate
  FEndDate =FEndDate
  #清空id表

  sql_df_truncate =paste0("truncate table rds_src_lp_customer_id_update")
  tsda::mysql_delete2(token =DF_token ,sql_str = sql_df_truncate)

  print("清空表数据")



  sql_lp_id=  paste0("select id,updated_at from  t_customer_customers
where cast(FROM_UNIXTIME(updated_at) as date) >='",FStartDate,"'
and   cast(FROM_UNIXTIME(updated_at) as date)<='",FEndDate,"'
        ")
  data_lp_id =  tsda::mysql_select2(token = LP_token,sql  = sql_lp_id)


  data_lp_id = as.data.frame(data_lp_id)
  data_lp_id = tsdo::na_standard(data_lp_id)

  #查看该更新日期内行数
  count =nrow(data_lp_id)
  print(count)

  #插入DF的客户id更新日期表

  tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_src_lp_customer_id_update',r_object = data_lp_id,append = TRUE)


  #删除id表已存在的id和日期

  sql_df_id_update =paste0("
INSERT OVERWRITE TABLE rds_src_lp_customer_id_update
SELECT a.*
FROM rds_src_lp_customer_id_update a
LEFT JOIN rds_oms_lpro_src_t_customer_customers b
    ON a.id = b.id AND a.updated_at = b.updated_at
WHERE b.id IS NULL;
")

  tsda::mysql_update2(token = DF_token,sql_str = sql_df_id_update)


  #删除df表需要更新的数据

  sql_df_customer=paste0("INSERT OVERWRITE TABLE rds_oms_lpro_src_t_customer_customers
SELECT *
FROM rds_oms_lpro_src_t_customer_customers
WHERE id NOT IN (SELECT id FROM rds_src_lp_customer_id_update);")
  tsda::mysql_update2(token = DF_token,sql_str = sql_df_customer)



  #查询剩下需要同步的id


  sql_df_id_view = paste0("select id from rds_src_lp_customer_id_update")

  data_df_id_view = tsda::mysql_select2(token = DF_token,sql = sql_df_id_view)


  count = nrow(data_df_id_view)
  print(count)



  if(count==0){

    res = print(paste0("表t_customer_customers同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  }else{

    #查询id对应的linkpro中的数据

    id_list <- paste(data_df_id_view$id, collapse = ",")
    sql_lp_customer <- paste0("SELECT * FROM t_customer_customers where id in (", id_list, ")")

    data_lp_customer = tsda::mysql_select2(token =LP_token ,sql = sql_lp_customer)

    data_lp_customer = as.data.frame(data_lp_customer)
    data_lp_customer = tsdo::na_standard(data_lp_customer)
    res =tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_oms_lpro_src_t_customer_customers',r_object = data_lp_customer,append = TRUE)

    print(paste0("表t_customer_customers同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))



  }







  return(res)

}
