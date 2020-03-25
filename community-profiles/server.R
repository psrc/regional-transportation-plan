# Define server logic required to draw the map for the main panel
shinyServer(function(input, output) {

    output$general_heading <- renderText({
        paste(input$Place, " in the Regional Transportation Plan.")
    })

    output$summary_stat_heading <- renderText({
        paste("Summary Statistics (", min_year, "/", max_year ,"):")
    })
    
    output$age_heading <- renderText({
        paste(input$Place, ": Household population by age group")
    })

    output$gender_heading <- renderText({
        paste(input$Place, ": Household population by gender")
    })

    output$worker_heading <- renderText({
        paste(input$Place, ": Household population by work status")
    })    

    output$student_heading <- renderText({
        paste(input$Place, ": Household population by student status")
    }) 

    output$hhtype_heading <- renderText({
        paste(input$Place, ": Households by type of housing")
    }) 

    output$hhincome_heading <- renderText({
        paste(input$Place, ": Households by income of household")
    }) 

    output$hhownership_heading <- renderText({
        paste(input$Place, ": Households by ownersip")
    })

    output$hhsize_heading <- renderText({
        paste(input$Place, ": Households by size")
    })

    output$occupation_heading <- renderText({
        paste(input$Place, ": Jobs by sector")
    })

    output$work_mode_heading <- renderText({
        paste(input$Place, ": Commute trips by mode")
    })
                            
    output$Population <- renderText({
        paste("Population: ",  format(return_estimate(model.outputs,input$Place,"total","all", "", "total-population", "Total-Population", min_year,0), nsmall = 0, big.mark = ","), "/", format(return_estimate(model.outputs,input$Place,"total","all", "", "total-population", "Total-Population", max_year,0), nsmall = 0, big.mark = ","))
    })

    output$Households <- renderText({
        paste("Households: ",  format(return_estimate(model.outputs,input$Place,"total","all", "", "total-households", "Total-Households", min_year,0), nsmall = 0, big.mark = ","), "/", format(return_estimate(model.outputs,input$Place,"total","all", "", "total-households", "Total-Households",  max_year,0), nsmall = 0, big.mark = ","))
    })
    
    output$Jobs <- renderText({
        paste("Jobs: ",  format(return_estimate(model.outputs,input$Place,"total","all", "", "total-jobs", "Total-Jobs", min_year,0), nsmall = 0, big.mark = ","), "/", format(return_estimate(model.outputs,input$Place,"total","all", "", "total-jobs", "Total-Jobs",  max_year,0), nsmall = 0, big.mark = ","))
    })

    output$HHSize <- renderText({
        paste("Household Size: ", round(return_estimate(model.outputs,input$Place,"total","all", "", "total-population", "Total-Population", min_year,0)/return_estimate(model.outputs,input$Place,"total","all", "", "total-households", "Total-Households", min_year,0), 2), "/", round(return_estimate(model.outputs,input$Place,"total","all", "", "total-population", "Total-Population",  max_year,0)/return_estimate(model.outputs,input$Place,"total","all", "", "total-households", "Total-Households",  max_year,0), 2))
    })    
    
    output$ModelBackground <- renderText({
        paste("SoundCast is a travel demand model system built for the Puget Sound Region and is the main analytical tool for the Regional Transportation Plan. The model was designed to depict diverse human travel behavior and include travel sensitivity to land use and the built environment. SoundCast outputs transportation network measures such as highway volumes in one hour periods in a future year or number of boardings on a transit line. It also outputs people's travel choices like average trip distances or how many bicycle trips they will take.")
    })
    
    output$ActivityModelBackground <-renderText({
        paste("An activity-based model system is a highly disaggregate environment, representing the travel behavior of each individual and household separately. Disaggregation avoids the errors and biases associated with generalization and averaging that plague trip-based modeling and lends itself to a more realistic and accurate portrayal of travel behavior and demand.")
        
    })

    output$ModelLink <- renderText({
        paste("For more information on SoundCast, please visit https://www.psrc.org/activity-based-travel-model-soundcast")
    })
    
    output$place_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = community.shape[which(community.shape$NAME %in% input$Place),],
                        fillColor = "76787A",
                        weight = 4,
                        opacity = 1.0,
                        color = "#91268F",
                        dashArray = "4",
                        fillOpacity = 0.0)%>%
            setView(lng=find_place_data(input$Place,"INTPTLON"), lat=find_place_data(input$Place,"INTPTLAT"), zoom=find_place_data(input$Place,"ZOOM"))
    })
        
    output$table_mode_to_work <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all",modes,"commute-trips","total-trips", modes_cols, modes_order,modes_length, mode_name)})
    output$chart_mode_to_work <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all",modes,"commute-trips","total-trips", modes_cols, modes_order,modes_length, mode_name, "Commute Trips")})
    output$map_mode_to_work <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all",input$Mode_Var,"commute-trips","total-trips",input$Mode_Year , "Blues", paste0(input$Mode_Var ," to Work"), "", "%")})

    output$table_occupation <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","jobs",jobs, jobs_cols, jobs_order,jobs_length, jobs_name)})
    output$chart_occupation <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","jobs",jobs, jobs_cols, jobs_order,jobs_length, jobs_name, "Jobs")})
    output$map_occupation <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","jobs",input$Job_Var, input$Job_Year , "Blues", paste0(input$Job_Var ," jobs"), "", "%")})

    output$table_age <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","age",age, age_cols, age_order,age_length, age_name)})
    output$chart_age <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","age",age, age_cols, age_order,age_length, age_name, "Household Population")})
    output$map_age <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","age",input$Age_Var, input$Age_Year , "Blues", paste0("People ",input$Age_Var), "", "%")})

    output$table_gender <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","gender",gender, gender_cols, gender_order,gender_length, gender_name)})
    output$chart_gender <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","gender",gender, gender_cols, gender_order,gender_length, gender_name, "Household Population")})
    output$map_gender <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","gender",input$Gender_Var, input$Gender_Year , "Blues", paste0("People ",input$Gender_Var), "", "%")})
    
    output$table_worker <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","worker",worker, worker_cols, worker_order,worker_length, worker_name)})
    output$chart_worker <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","worker",worker, worker_cols, worker_order,worker_length, worker_name, "Household Population")})
    output$map_worker <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","worker",input$Worker_Var, input$Worker_Year , "Blues", paste0(input$Worker_Var), "", "%")})

    output$table_student <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","student",student, student_cols, student_order,student_length, student_name)})
    output$chart_student <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","student",student, student_cols, student_order,student_length, student_name, "Household Population")})
    output$map_student <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","student",input$Student_Var, input$Student_Year , "Blues", paste0(input$Student_Var), "", "%")})

    output$table_hhsize <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","household-size",hhsize, hhsize_cols, hhsize_order,hhsize_length, hhsize_name)})
    output$chart_hhsize <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","household-size",hhsize, hhsize_cols, hhsize_order,hhsize_length, hhsize_name, "Total Households")})
    output$map_hhsize <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-size",input$HHSize_Var, input$HHSize_Year , "Blues", paste0(input$HHSize_Var), "", "%")})

    output$table_hhownership <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","household-ownership",hhownership, hhownership_cols, hhownership_order,hhownership_length, hhownership_name)})
    output$chart_hhownership <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","household-ownership",hhownership, hhownership_cols, hhownership_order,hhownership_length, hhownership_name, "Total Households")})
    output$map_hhownership <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-ownership",input$HHOwn_Var, input$HHOwn_Year , "Blues", paste0(input$HHOwn_Var), "", "%")})

    output$table_hhtype <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","household-type",hhtype, hhtype_cols, hhtype_order,hhtype_length, hhtype_name)})
    output$chart_hhtype <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","household-type",hhtype, hhtype_cols, hhtype_order,hhtype_length, hhtype_name, "Total Households")})
    output$map_hhtype <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-type",input$HHType_Var, input$HHType_Year , "Blues", paste0(input$HHType_Var), "", "%")})

    output$table_hhincome <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all","","household-income",hhincome, hhincome_cols, hhincome_order,hhincome_length, hhincome_name)})
    output$chart_hhincome <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all","","household-income",hhincome, hhincome_cols, hhincome_order,hhincome_length, hhincome_name, "Total Households")})
    output$map_hhincome <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-income",input$HHIncome_Var, input$HHIncome_Year , "Blues", paste0(input$HHIncome_Var), "", "%")})

    output$table_work_traveltime <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$estimate_choice,"all",input$TT_Mode,"commute-time",traveltime, traveltime_cols, traveltime_order,traveltime_length, traveltime_name)})
    output$chart_work_traveltime <- renderPlotly({create_stacked_bar(model.outputs,input$Place,input$estimate_choice,"all",input$TT_Mode,"commute-time",traveltime, traveltime_cols, traveltime_order,traveltime_length, traveltime_name, "Commute Trips")})
    output$map_work_traveltime <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all",input$TT_Mode,"commute-time",input$TT_Var, input$TT_Year , "Blues", paste0(input$TT_Var), "", "%")})
    
    observeEvent(input$showpanel, {
        
        if(input$showpanel == TRUE) {
            removeCssClass("Main", "col-sm-12")
            addCssClass("Main", "col-sm-8")
            shinyjs::show(id = "sidebar")
            shinyjs::enable(id = "sidebar")
        }
        else {
            removeCssClass("Main", "col-sm-8")
            addCssClass("Main", "col-sm-12")
            shinyjs::hide(id = "sidebar")
        }
    })
    
    
})
