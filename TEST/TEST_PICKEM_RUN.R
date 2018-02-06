imported_salaries <- import_salaries(sal_path = '/Users/ashleyvaughan/Downloads/DKSalaries_20180205_pickem.csv',
                                     from_entry = TRUE)
salary_lu <- make_salary_lu(imported_salaries, 
                            from_entry = TRUE, 
                            game_style = "pickem")
imported_projections <- import_projections()
augmented_projections <- augment_projections(imported_projections)
aug_proj_plus_salid <- add_salary_ids(projections = augmented_projections, salaries = salary_lu)

