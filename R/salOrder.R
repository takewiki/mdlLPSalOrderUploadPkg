#' 销售订单主表更新
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
#' salOrder_sync
salOrder_sync <- function(dms_token,erp_token,FStartDate,FEndDate) {

  print(paste0("表t_order_orders同步开始，开始时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  DF_token = dms_token
  LP_token = erp_token
  FStartDate=FStartDate
  FEndDate =FEndDate


  sql_df_truncate =paste0("truncate table rds_src_lp_order_id_update")
  tes = tsda::mysql_delete2(token =DF_token ,sql_str = sql_df_truncate)

  print("清空表数据")

  sql_lp_id=  paste0("select id,updated_at from  t_order_orders
where cast(FROM_UNIXTIME(updated_at) as date) >='",FStartDate,"'
and  cast(FROM_UNIXTIME(updated_at) as date)<='",FEndDate,"'
        ")
  data_lp_id =  tsda::mysql_select2(token = LP_token,sql  = sql_lp_id)


  data_lp_id = as.data.frame(data_lp_id)
  data_lp_id = tsdo::na_standard(data_lp_id)

  #查看该更新日期内行数
  count =nrow(data_lp_id)
  print(count)

  #插入DF的销售订单id更新日期表

  tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_src_lp_order_id_update',r_object = data_lp_id,append = TRUE)



  #删除id表已存在的id和日期

  sql_df_id_update =paste0("
INSERT OVERWRITE TABLE rds_src_lp_order_id_update
SELECT a.*
FROM rds_src_lp_order_id_update a
LEFT JOIN rds_oms_lpro_src_t_order_orders b
    ON a.id = b.id AND a.updated_at = b.updated_at
WHERE b.id IS NULL;
")

  tsda::mysql_update2(token = DF_token,sql_str = sql_df_id_update)


  #删除df表需要更新的数据

  sql_df_order=paste0("INSERT OVERWRITE TABLE rds_oms_lpro_src_t_order_orders
SELECT *
FROM rds_oms_lpro_src_t_order_orders
WHERE id NOT IN (SELECT id FROM rds_src_lp_order_id_update);")
  tsda::mysql_update2(token = DF_token,sql_str = sql_df_order)



  #查询剩下需要同步的id


  sql_df_id_view = paste0("select id from rds_src_lp_order_id_update")

  data_df_id_view = tsda::mysql_select2(token = DF_token,sql = sql_df_id_view)


  #查看该更新日期内行数
  count =nrow(data_df_id_view)
  print(count)

  if(count==0){

    res=print(paste0("表t_order_orders同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  }else{

    #查询id对应的linkpro中的数据

    id_list <- paste(data_df_id_view$id, collapse = ",")
    sql_lp_order <- paste0("SELECT * FROM t_order_orders where id in (", id_list, ")")



    data_lp_order = tsda::mysql_select2(token =LP_token ,sql = sql_lp_order)

    data_lp_order = as.data.frame(data_lp_order)
    data_lp_order = tsdo::na_standard(data_lp_order)



    res =tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_oms_lpro_src_t_order_orders',r_object = data_lp_order,append = TRUE)


    print(paste0("表t_order_orders同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))



  }





  return(res)

}


#' 销售订单查询
#'
#' @param dms_token 第二个参数
#' @param FStartDate
#' @param FEndDate
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' salOrder_view
salOrder_view <- function(dms_token,FStartDate,FEndDate) {


  sql1=paste0("truncate table rds_oms_lpro_src_t_sal_saleorder")
  tsda::mysql_delete2(token =dms_token ,sql_str =sql1 )



  sql2=paste0("truncate table rds_oms_lpro_src_t_sal_saleorderentry")
  tsda::mysql_delete2(token =dms_token ,sql_str =sql2)


  sql3=paste0("insert into rds_oms_lpro_src_t_sal_saleorder
select * from rds_oms_lpro_src_vw_sal_saleorder_en
")
  tsda::mysql_update2(token = dms_token,sql_str =sql3 )


  sql4=paste0("
insert into rds_oms_lpro_src_t_sal_saleorderentry
select * from rds_oms_lpro_src_vw_sal_saleorderEntry_en
")
  tsda::mysql_update2(token = dms_token,sql_str =sql4 )







  sql =paste0("
SELECT
    a.FID AS `销售订单表头内码`,
    a.FOrderNo AS `订单编号`,
    a.FOriginalOrderNo AS `原订单编号`,
    a.FLogisticsOrderNumber AS `物流订单编号`,
    a.FOrderCurrency AS `原货币`,                       -- 主表原货币
    a.FOrderTotal AS `订单总金额`,
    a.FGoodsTotal AS `商品总金额`,
    a.FFreight AS `配送金额`,
    a.FDiscountType AS `折扣类型`,
    a.FDiscount AS `折扣`,                              -- 主表折扣
    a.FPlatformOrderTotal AS `平台订单金额`,
    a.FDeclaredValue AS `申报价值`,
    a.FTaxRate AS `税金`,                              -- 主表税率
    a.FSalesmanName AS `业务员`,
    a.FOrderType AS `订单类型`,
    a.FPaymentMethod AS `收款方式`,
    a.FPayWay AS `收款方式2`,
    a.FDistributorName AS `分销商`,
    a.FCompanyCurrency AS `本位币`,                    -- 主表本位币
    a.FExchangeRate AS `汇率`,
    a.FCompanyTotal AS `本位币价格`,
    a.FCarrierName AS `物流公司`,
    a.FLogisticsCode AS `物流编码`,
    a.FOverPay AS `余额支付`,
    a.FOrderDate AS `下单日期`,
    a.FCustomerName AS `客户名称`,
    a.FCustomerCode AS `客户编码`,
    a.FCustomerCategoryName AS `客户类别`,
    a.FCustomerAccount AS `客户账号`,
    a.FCustomerPhone AS `手机号`,
    a.FCustomerTelephone AS `电话`,
    a.FReceiverName AS `收货人姓名`,
    a.FReceiverTelphone AS `固定电话`,
    a.FReceiverPhone AS `手机号码`,
    a.FReceiverCompany AS `公司`,
    a.FReceiverZip AS `邮政编码`,
    a.FReceiverEmail AS `Email`,
    a.FCountry AS `国家`,
    a.FProvince AS `省`,
    a.FCity AS `市`,
    a.FArea AS `区`,
    a.FAddress AS `详细地址`,
    a.FRemarks AS `卖家备注`,
    a.FMemberRemarks AS `买家备注`,
    a.FAbnormalRemarks AS `异常备注`,
    b.FEntryID AS `销售订单表体内码`,
    b.FProductName AS `商品名称`,
    b.FProductCode AS `商品编码`,
    b.FItemCode AS `物料编码`,
    b.FChannelItemCode AS `渠道物料编码`,
    b.FOrderCurrency AS `原货币_明细`,                 -- 明细表原货币（单独别名）
    b.FCompanyCurrency AS `本位币_明细`,               -- 明细表本位币（单独别名）
    b.FSellPrice AS `商品售价`,
    b.FSellPriceInCompanyCurrency AS `商品售价_本位币`,
    b.FPurchasePrice AS `采购价`,
    b.FCostPrice AS `成本价`,
    b.FPurchaseQuantity AS `购买数量`,
    b.FDiscount AS `折扣_明细`,                        -- 明细表折扣
    b.FTaxRate AS `税率_明细`,                         -- 明细表税率
    b.FVariant AS `规格`,
    b.FType AS `类型`,
    b.FTag AS `标签`,
    b.FNetWeight AS `商品净重`,
    b.FGrossWeight AS `商品毛重`
FROM rds_oms_lpro_src_t_sal_saleorder a
INNER JOIN rds_oms_lpro_src_t_sal_saleorderentry b ON a.FID = b.FID
where cast(a.FOrderDate  as date)>= '",FStartDate,"'   and  cast(a.FOrderDate  as date)<='",FEndDate,"'
")

  res = tsda::mysql_select2(token = dms_token,sql = sql)


  return(res)

}





#' 订单地址表更新
#'
#' @param dms_token 第二个参数
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' ordersAddress_sync
ordersAddress_sync <- function(dms_token,erp_token) {

  print(paste0("表t_order_orders_address同步开始，开始时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  DF_token = dms_token
  LP_token = erp_token



  #删除df表需要更新的数据

  sql_df_order=paste0("INSERT OVERWRITE TABLE rds_oms_lpro_src_t_order_orders_address
SELECT *
FROM rds_oms_lpro_src_t_order_orders_address
WHERE order_id NOT IN (SELECT id FROM rds_src_lp_order_id_update);")
  tsda::mysql_update2(token = DF_token,sql_str = sql_df_order)


  #查询剩下需要同步的id


  sql_df_id_view = paste0("select id from rds_src_lp_order_id_update")

  data_df_id_view = tsda::mysql_select2(token = DF_token,sql = sql_df_id_view)


  count=nrow(data_df_id_view)

  print(count)


  if(count==0){


    res=print(paste0("表t_order_orders_address同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  }else{


    #查询id对应的linkpro中的数据

    id_list <- paste(data_df_id_view$id, collapse = ",")
    sql_lp_order <- paste0("select  distinct id,	order_id,	customer_address_id,name,	phone,	telphone,email,	country_id,	province_id,city,	area,
address	,complete_address,	zip,language,	company_name from t_order_orders_address
 where    order_id in (", id_list, ")")



    data_lp_order = tsda::mysql_select2(token =LP_token ,sql = sql_lp_order)

    data_lp_order = as.data.frame(data_lp_order)
    data_lp_order = tsdo::na_standard(data_lp_order)



    res =tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_oms_lpro_src_t_order_orders_address',r_object = data_lp_order,append = TRUE)


    print(paste0("表t_order_orders_address同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  }





  return(res)

}






#' 订单日志表更新
#'
#' @param dms_token 第二个参数
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' ordersRemarks_sync
ordersRemarks_sync <- function(dms_token,erp_token) {

  print(paste0("表t_order_orders_has_remarks同步开始，开始时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  DF_token = dms_token
  LP_token = erp_token


  count_min=paste0("select max(id) from rds_oms_lpro_src_t_order_orders_has_remarks")
  count_min = tsda::mysql_select2(token = DF_token,sql  = count_min)
  count_max = paste0("select max(id) from t_order_orders_has_remarks")
  count_max = tsda::mysql_select2(token = LP_token,sql  = count_max)

  data_lp = paste0("select * from t_order_orders_has_remarks where id >",count_min," and id <=",count_max,"")


  data_lp =  tsda::mysql_select2(token = LP_token,sql  = data_lp)

  data_lp = as.data.frame(data_lp)
  data_lp = tsdo::na_standard(data_lp)



  #print(data_lp)

  res = tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_oms_lpro_src_t_order_orders_has_remarks',r_object = data_lp,append = TRUE)

  print(paste0("表t_order_orders_has_remarks同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  return(res)

}




#' 订单明细表更新
#'
#' @param dms_token 第二个参数
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' ordersItems_sync
ordersItems_sync <- function(dms_token,erp_token) {

  print(paste0("表t_order_orders_items同步开始，开始时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  DF_token = dms_token
  LP_token = erp_token


  #删除df表需要更新的数据

  sql_df_order=paste0("INSERT OVERWRITE TABLE rds_oms_lpro_src_t_order_orders_items
SELECT *
FROM rds_oms_lpro_src_t_order_orders_items
WHERE order_id NOT IN (SELECT id FROM rds_src_lp_order_id_update);")
  tsda::mysql_update2(token = DF_token,sql_str = sql_df_order)


  #查询剩下需要同步的id


  sql_df_id_view = paste0("select id from rds_src_lp_order_id_update")

  data_df_id_view = tsda::mysql_select2(token = DF_token,sql = sql_df_id_view)


  count = nrow(data_df_id_view)
  print(count)

  if(count==0){

    res = print(paste0("表t_order_orders_items同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))

  }else{


    #查询id对应的linkpro中的数据

    id_list <- paste(data_df_id_view$id, collapse = ",")
    sql_lp_order <- paste0("select * from t_order_orders_items  where    order_id in (", id_list, ")")
    data_lp_order = tsda::mysql_select2(token =LP_token ,sql = sql_lp_order)

    data_lp_order = as.data.frame(data_lp_order)
    data_lp_order = tsdo::na_standard(data_lp_order)
    res = tsda::mysql_writeTable2(token = DF_token,table_name = 'rds_oms_lpro_src_t_order_orders_items',r_object = data_lp_order,append = TRUE)

    print(paste0("表t_order_orders_items同步结束，结束时间：", format(Sys.time(), tz = "Asia/Shanghai")))


  }


  return(res)

}


