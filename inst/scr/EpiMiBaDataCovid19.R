#' Extract influenza and SARS-CoV-2 tests from EpiMiBa.
#'
#' @param StartDate Start date.
#' @param EndDate End data.
#' @import ISOweek
#' @import RODBC
#' @return data with tests for influenza (A, H1N1, H3N2, B) and COVID19
GetMiBaData <- function(StartDate, EndDate) {

  # Read from EpiMiBa
  con <- RODBC::odbcConnect("EpiMiBa06", readOnlyOptimize = TRUE)
  InflData <- RODBC::sqlQuery(con, paste0("
     with
    	MiBa as
      (select replace(Header_Cprnr, '-', '') as cprnr, Header_Prdate as prdate,
    		case when (upper(TabAnalysis_Text) like 'INFLUENZA A%') and (upper(Quantitative_Comment) not like '%TYPE B%') and (upper(Quantitative_Comment) not like '%INFLUENZA B%') and
    		           ((upper(Header_Evalutation) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%IKKE P%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%NEGATIV%') or (upper(Quantitative_EvaluationText) like '%IKKE P%') or (upper(Quantitative_EvaluationText) like '%DNA/RNA IK%') or
    		            (upper(CommentText) like '%NEGATIV%') or (upper(CommentText) like '%IKKE P%') or (upper(CommentText) like '%DNA/RNA IK%')) and
    		          ((upper(TabAnalysis_Text) not like '%H1%') and (upper(Quantitative_Quantity) not like '%H1%') and
    		            (upper(Quantitative_EvaluationText) not like '%H1%') and (upper(Quantitative_Comment) not like '%H1%') and ((CommentText is NULL) or (upper(CommentText) not like '%H1%'))) and
    		           ((upper(TabAnalysis_Text) not like '%H3%') and (upper(Quantitative_Quantity) not like '%H3%') and
    		            (upper(Quantitative_EvaluationText) not like '%H3%') and (upper(Quantitative_Comment) not like '%H3%') and ((CommentText is NULL) or (upper(CommentText) not like '%H3%')))
    				    then 0
    		     when (upper(TabAnalysis_Text) like 'INFLUENZA A%') and (upper(Quantitative_Comment) not like '%TYPE B%') and (upper(Quantitative_Comment) not like '%INFLUENZA B%') and
    		           ((upper(Header_Evalutation) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%PÅ%') or (upper(Quantitative_Quantity) not like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%POSITIV%') or (upper(Quantitative_EvaluationText) like '%PÅ%') or
    		            (upper(CommentText) like '%POSITIV%') or (upper(CommentText) like '%PÅ%')) and
    		          ((upper(TabAnalysis_Text) not like '%H1%') and (upper(Quantitative_Quantity) not like '%H1%') and
    		            (upper(Quantitative_EvaluationText) not like '%H1%') and (upper(Quantitative_Comment) not like '%H1%') and ((CommentText is NULL) or (upper(CommentText) not like '%H1%'))) and
    		           ((upper(TabAnalysis_Text) not like '%H3%') and (upper(Quantitative_Quantity) not like '%H3%') and
    		            (upper(Quantitative_EvaluationText) not like '%H3%') and (upper(Quantitative_Comment) not like '%H3%') and ((CommentText is NULL) or (upper(CommentText) not like '%H3%')))
    				    then 1
    			   else NULL end as A,
    		case when (upper(TabAnalysis_Text) like 'INFLUENZA A%') and
    		           ((upper(Header_Evalutation) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%IKKE P%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%NEGATIV%') or (upper(Quantitative_EvaluationText) like '%IKKE P%') or (upper(Quantitative_EvaluationText) like '%DNA/RNA IK%') or
    		            (upper(CommentText) like '%NEGATIV%') or (upper(CommentText) like '%IKKE P%') or (upper(CommentText) like '%DNA/RNA IK%')) and
    		           ((upper(TabAnalysis_Text) like '%H1%') or (upper(Quantitative_Quantity) like '%H1%') or
    		            (upper(Quantitative_EvaluationText) like '%H1%') or (upper(Quantitative_Comment) like '%H1%') or (upper(CommentText) like '%H1%')) and
    		           ((upper(TabAnalysis_Text) not like '%H3%') and (upper(Quantitative_Quantity) not like '%H3%') and
    		            (upper(Quantitative_EvaluationText) not like '%H3%') and (upper(Quantitative_Comment) not like '%H3%') and ((CommentText is NULL) or (upper(CommentText) not like '%H1%')))
    				    then 0
    		     when (upper(TabAnalysis_Text) like 'INFLUENZA A%') and
    		           ((upper(Header_Evalutation) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%PÅVIST%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%POSITIV%') or (upper(Quantitative_EvaluationText) like '%PÅ%') or
    		            (upper(CommentText) like '%POSITIV%') or (upper(CommentText) like '%PÅ%')) and
    		           ((upper(TabAnalysis_Text) like '%H1%') or (upper(Quantitative_Quantity) like '%H1%') or
    		            (upper(Quantitative_EvaluationText) like '%H1%') or (upper(Quantitative_Comment) like '%H1%') or (upper(CommentText) like '%H1%'))
    				    then 1
    			   else NULL end as H1N1,
    		case when (upper(TabAnalysis_Text) like 'INFLUENZA A%') and
    		           ((upper(Header_Evalutation) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%IKKE P%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%NEGATIV%') or (upper(Quantitative_EvaluationText) like '%IKKE P%') or (upper(Quantitative_EvaluationText) like '%DNA/RNA IK%') or
    		            (upper(CommentText) like '%NEGATIV%') or (upper(CommentText) like '%IKKE P%') or (upper(CommentText) like '%DNA/RNA IK%')) and
    		           ((upper(TabAnalysis_Text) like '%H3%') or (upper(Quantitative_Quantity) like '%H3%') or
    		            (upper(Quantitative_EvaluationText) like '%H3%') or (upper(Quantitative_Comment) like '%H3%') or (upper(CommentText) like '%H3%'))
    				    then 0
    		     when (upper(TabAnalysis_Text) like 'INFLUENZA A%') and
    		           ((upper(Header_Evalutation) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%PÅVIST%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%POSITIV%') or (upper(Quantitative_EvaluationText) like '%PÅ%') or
    		            (upper(CommentText) like '%POSITIV%') or (upper(CommentText) like '%PÅ%')) and
    		           ((upper(TabAnalysis_Text) like '%H3%') or (upper(Quantitative_Quantity) like '%H3%') or
    		            (upper(Quantitative_EvaluationText) like '%H3%') or (upper(Quantitative_Comment) like '%H3%') or (upper(CommentText) like '%H3%'))
    				    then 1
    			   else NULL end as H3N2,
    		case when ((upper(TabAnalysis_Text) like '%INFLUENZA B%') or (upper(TabAnalysis_Text) like '%B VIRUS%') or
						       (upper(Quantitative_Comment) like '%TYPE B%') or (upper(Quantitative_Comment) like '%INFLUENZA B%')) and
    		           ((upper(Header_Evalutation) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%IKKE P%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%NEGATIV%') or (upper(Quantitative_EvaluationText) like '%IKKE P%') or (upper(Quantitative_EvaluationText) like '%DNA/RNA IK%') or
    		            (upper(CommentText) like '%NEGATIV%') or (upper(CommentText) like '%IKKE P%') or (upper(CommentText) like '%DNA/RNA IK%'))
    				    then 0
    		     when ((upper(TabAnalysis_Text) like 'INFLUENZA B%') or (upper(TabAnalysis_Text) like 'B VIRUS%') or
						        (upper(Quantitative_Comment) like '%TYPE B%') or (upper(Quantitative_Comment) like '%INFLUENZA B%')) and
    		           ((upper(Header_Evalutation) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%PÅVIST%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%POSITIV%') or (upper(Quantitative_EvaluationText) like '%PÅ%') or
    		            (upper(CommentText) like '%POSITIV%') or (upper(CommentText) like '%PÅ%'))
    				    then 1
    			   else NULL end as B,
    		case when ((upper(TabAnalysis_Text) like '%SARS%') or (upper(Quantitative_Comment) like '%nCOV%')) and
    		           ((upper(Header_Evalutation) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%NEGATIV%') or (upper(Quantitative_Quantity) like '%IKKE P%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%NEGATIV%') or (upper(Quantitative_EvaluationText) like '%IKKE P%') or (upper(Quantitative_EvaluationText) like '%DNA/RNA IK%') or
    		            (upper(CommentText) like '%NEGATIV%') or (upper(CommentText) like '%IKKE P%') or (upper(CommentText) like '%DNA/RNA IK%'))
    				    then 0
    		     when ((upper(TabAnalysis_Text) like '%SARS%') or (upper(Quantitative_Comment) like '%nCOV%')) and
    		           ((upper(Header_Evalutation) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%POSITIV%') or (upper(Quantitative_Quantity) like '%PÅVIST%') or (upper(Quantitative_Quantity) like '%DNA/RNA IK%') or
    		            (upper(Quantitative_EvaluationText) like '%POSITIV%') or (upper(Quantitative_EvaluationText) like '%PÅ%') or
    		            (upper(CommentText) like '%POSITIV%') or (upper(CommentText) like '%PÅ%'))
    				    then 1
    			   else NULL end as COVID19,
    			   TabLabSection_Text, TabAnalysis_Text
    		from
    		IB_EpiMiBa.output.vw_INFLUENZA_Covid19_samlet with(nolock)
        where ('", StartDate, "' <= header_prdate) and (cast(header_prdate as date) <= '", EndDate, "') and (upper(Header_Cprnr) not like '%TEST%') and
    		(upper(Quantitative_quantity) not in('INKONKLUSIV','PRØVEN ER ..','UBEDØMMELIG,','UBEDÖMMELIG,','EJ UDFØRT')) and
    		(upper(replace(Quantitative_EvaluationText,': ','')) not in('IKKE UNDERSØGT','SVAR FØLGER','INKONKLUSIV','MISLYKKET','HÆMNING','HÆMMET','HÆMMET PCR REAKTION',
				'IKKE UDFØRT','RESULT TVIVLSOMT','RESULTAT FØLGER','SE BEMÆRKNING','RESULTATET KAN IKKE TOLKES','GRÄNSEVÄRDI','UBESTEMBAR'))
    	),
      MiBa2 as
      	(select cprnr, prdate, case when (H1N1 = 1) or (H3N2 = 1) then 1 else A end as A, H1N1, H3N2, B, COVID19, TabLabSection_Text, TabAnalysis_Text from MiBa
      ),
      MiBa3 as
      	(select cprnr, prdate,
      	  case when ((upper(TabAnalysis_Text)  like 'INFLUENZA A + B VIRUS%') or (upper(TabAnalysis_Text)  like 'INFLUENZA A+B VIRUS%')) and (B = 1) and (A is NULL)
      	          then 0
      	       else A end as A, H1N1, H3N2,
      	  case when ((upper(TabAnalysis_Text)  like 'INFLUENZA A + B VIRUS%') or (upper(TabAnalysis_Text)  like 'INFLUENZA A+B VIRUS%')) and (A = 1) and (B is NULL)
      	          then 0
      	       else B end as B,
      	  COVID19, TabLabSection_Text
      	  from MiBa2 where (A is not NULL) or (H1N1 is not NULL) or (H3N2 is not NULL) or (B is not NULL) or (COVID19 is not NULL)
      )

    	select cprnr, prdate, max(A) as A, max(H1N1) as H1N1, max(H3N2) as H3N2, max(B) as B, max(COVID19) as COVID19, TabLabSection_Text
    	from MiBa3
  		group by cprnr, prdate, TabLabSection_Text
  		order by cprnr, prdate, TabLabSection_Text
    "), stringsAsFactors = FALSE, as.is = TRUE)
  RODBC::odbcClose(con)
  rm(con)

  return(InflData)
}
