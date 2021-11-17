#' A function to compute a unpaired two-samples wilcoxon test for a TGCA dataset
#'
#' @param df dataframe with individuals in rows and cancer attributes in columns
#' @param cancer_colname header name matching patient's cancer status
#' @param patient_colname header name of matching patients' ID
#' @param cancer_type Actual types of cancer to be selected from (i.e., "ACC", "BRCA", etc...)
#' @param patients Actual patient's ID
#' @param metadata_col_num headername of numerical metadata colum (i.e., age, os.time)
#' @param metadata_col headername of categorical metadata column (i.e., gender, race, vital_status )
#' @param metadata_levels two values from the categorical metadata column to be compared (i.e., WHITE, ASIAN for race, or Stage1, stage4 for tumor stage)
#' @return dataframe corresponding of the sheet number and filtered based on header
#' @export
#'
#' @examples
#'
#' patho_age <- stat_test(data_tgca_tumor_stat, cancer_type =  c("BRCA") , metadata_col_num= "age_at_initial_pathologic_diagnosis",
#'                       metadata_col ="ajcc_pathologic_tumor_stage", metadata_levels=c("Stage I","Stage IV") )
#'
#' gender_age <- stat_test(data_tgca_tumor_stat, patients=data_tgca_tumor_stat$bcr_patient_barcode[grepl("TCGA*",data_tgca_tumor_stat$bcr_patient_barcode)], metadata_col_num= "age_at_initial_pathologic_diagnosis",
#'                        metadata_col ="gender", metadata_levels=c("FEMALE","MALE") )
#'
#' vita_ostime <- stat_test(data_tgca_tumor_stat, cancer_type = "BRCA", metadata_col_num= "OS.time",
#'                         metadata_col ="vital_status", metadata_levels=c("Dead","Alive") )
#'
#'
stat_test <- function(df=NULL, cancer_type=NULL, patients=NULL, metadata_col_num=NULL,metadata_col = NULL, metadata_levels=c(NULL,NULL)) {
  if (!is.null(patients) && !is.null(cancer_type)) {
    stop("filter dataset by either cancer type or patients samples")
  }

  if (!is.null(cancer_type)) {  assertthat::assert_that(is.character(cancer_type)) }
  if (!is.null(patients)) { assertthat::assert_that(is.character(patients)) }
  assertthat::assert_that(is.character(metadata_col_num))
  assertthat::assert_that(is.character(metadata_col))
  assertthat::assert_that(length(metadata_levels)==2)

  if (!is.null(cancer_type)) {
    w = df[df[,which(names(df)=="type")] %in% c(cancer_type) & df[,which(names(df)==metadata_col)] %in% c(metadata_levels),]

    res <- wilcox.test(w[,which(names(w)==metadata_col_num)] ~ w[,which(names(w)==metadata_col)])
  }
  if (!is.null(patients)){
    w = df[df[,which(names(df)=="bcr_patient_barcode")] %in% c(patients) & df[,which(names(df)==metadata_col)] %in% c(metadata_levels),]
    res <- wilcox.test(w[,which(names(w)==metadata_col_num)] ~ w[,which(names(w)==metadata_col)])
  }
  return(list(res$p.value))

}
