exclude_subjects <- function(data,
                             exclude_MRI = TRUE,
                             exclude_speakeng = TRUE,
                             exclude_psychdx = TRUE,
                             exclude_sud = TRUE,
                             exclude_asd = TRUE,
                             exclude_schiz = TRUE,
                             exclude_intdisab = TRUE,
                             exclude_birthcomp = TRUE,
                             exclude_dhx = TRUE,
exclude_gdiv = TRUE) {

# Load necessary tables from the ABCD data
sMRI_qc_excl <- read.csv("abcd-data-release-5.1/core/imaging/mri_y_qc_incl.csv")

screening <- read.csv("abcd-data-release-5.1/core/abcd-general/abcd_p_screen.csv")

dhx <- read.csv("abcd-data-release-5.1/core/physical-health/ph_p_dhx.csv")

p_demo_sel <- read.csv("abcd-data-release-5.1/core/abcd-general/abcd_p_demo.csv")
  
  # 1. Exclude based on MRI quality control

  if (exclude_MRI) {
    indeces_excl <- sMRI_qc_excl$imgincl_t1w == 0 & 
                    sMRI_qc_excl$imgincl_t2w == 0 & 
                    sMRI_qc_excl$imgincl_nback_include == 0
    subjects_excl_MRI <- sMRI_qc_excl$src_subject_id[indeces_excl]
  } else {
    subjects_excl_MRI <- character(0)
  }
  
  # 2. Exclude based on English fluency

  if (exclude_speakeng) {
    subjects_speakeng_excl <- screening$src_subject_id[!is.na(screening$scrn_speakeng) & 
                                                       (screening$scrn_speakeng == "0" | 
                                                        screening$scrn_speakeng == "3")]
  } else {
    subjects_speakeng_excl <- character(0)
  }
  
  # 3. Exclude based on mental health, substance use, or neurological disorders

  if (exclude_psychdx) {
    subjects_psychdx_excl <- screening$src_subject_id[!is.na(screening$scrn_psychdx_other) & 
                                                      screening$scrn_psychdx_other == 1]
  } else {
    subjects_psychdx_excl <- character(0)
  }
  
  if (exclude_sud) {
    subjects_sud_excl <- screening$src_subject_id[!is.na(screening$scrn_sud) & screening$scrn_sud == 1]
  } else {
    subjects_sud_excl <- character(0)
  }
  
  if (exclude_asd) {
    subjects_asd_excl <- screening$src_subject_id[!is.na(screening$scrn_asd) & screening$scrn_asd == 1]
  } else {
    subjects_asd_excl <- character(0)
  }
  
  if (exclude_schiz) {
    subjects_schiz_excl <- screening$src_subject_id[!is.na(screening$scrn_schiz) & screening$scrn_schiz == 1]
  } else {
    subjects_schiz_excl <- character(0)
  }
  
  if (exclude_intdisab) {
    subjects_intdisab_excl <- screening$src_subject_id[!is.na(screening$scrn_intdisab) & screening$scrn_intdisab == 1]
  } else {
    subjects_intdisab_excl <- character(0)
  }
  
  # 4. Exclude based on birth complications

  if (exclude_birthcomp) {
    subjects_birthcomp_excl <- screening$src_subject_id[!is.na(screening$scrn_birthcomp) & 
                                                        (screening$scrn_birthcomp == 1 | 
                                                         screening$scrn_birthcomp == 2)]
  } else {
    subjects_birthcomp_excl <- character(0)
  }
  
  # 5. Exclude based on premature birth

  if (exclude_dhx) {
    subjects_dhx_excl <- dhx$src_subject_id[!is.na(dhx$devhx_12a_p) & dhx$devhx_12a_p == 1]
  } else {
    subjects_dhx_excl <- character(0)
  }
  
  # 6. Exclude subjects whose assigned birthsex is not male or female
  if (exclude_gdiv) {
    subjects_gdiv_excl <- p_demo_sel$src_subject_id[!is.na(p_demo_sel$demo_sex_v2) & p_demo_sel$demo_sex_v2 == 3]
  } else {
    subjects_gdiv_excl <- character(0)
  }


  # Combine all exclusion subjects
  combined_subjects_excl <- unique(c(subjects_excl_MRI, subjects_speakeng_excl, subjects_psychdx_excl,
                                     subjects_sud_excl, subjects_asd_excl, subjects_schiz_excl,
                                     subjects_intdisab_excl, subjects_birthcomp_excl, subjects_dhx_excl, subjects_gdiv_excl))
  
  # Exclude subjects from the dataset
  data <- data[which(!data$src_subject_id %in% combined_subjects_excl), ]
  
  return(data)
}
