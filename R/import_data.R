#' A function to import data from TGCA dataset
#'
#' Apply batch effect correction using function ComBat
#'
#' @param df dataframe with individuals in rows and cancer attributes in columns
#' @param sheet_numb index of the sheet from excel file
#' @param headername header name of column we want to filter (no NA present)
#' @param tumor_value1 Known tumor value1
#' @param tumor_value2 Known tumor value2
#' @return dataframe corresponding of the sheet number selected and individuals with known tumor status
#' @export
#'
#' @examples
#'
#' data_tgca_tumor_stat <- run_import_data(df_name_and_path,1,"tumor_status","TUMOR FREE","WITH TUMOR")


#import data
run_import_data <- function(df=NULL, sheet_numb=NULL, headername=NULL, tumor_value1=NULL, tumor_value2=NULL) {
  xfun::strings_please()
  data_tgca <- openxlsx::read.xlsx(df , sheet=sheet_numb,rowNames=TRUE)
  data_tgca_tumor_stat <- data_tgca [ data_tgca[,which(names(data_tgca)==headername),]== tumor_value1 | data_tgca[,which(names(data_tgca)==headername),] == tumor_value2,]
  return(data_tgca_tumor_stat)
}
