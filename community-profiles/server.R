# Define server logic required to draw the map for the main panel
shinyServer(function(input, output) {

    output$general_heading <- renderText({
        paste(input$Place, "")
    })

    output$summary_stat_heading <- renderText({
        paste("Summary Statistics (", model_base_year, "/", model_horizon_year ,"):")
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

    # Summary Statistics
    output$Population <- renderText({paste("Population: ",  format(return_estimate(model.outputs, input$Place, "total-population", "Total-Population", model_base_year, "acs/acs5"), nsmall = 0, big.mark = ","), "/", format(return_estimate(model.outputs, input$Place, "total-population", "Total-Population", model_horizon_year, "UrbanSim"), nsmall = 0, big.mark = ","))})
    output$Households <- renderText({paste("Households: ",  format(return_estimate(model.outputs, input$Place, "total-households", "Total-Households", model_base_year, "acs/acs5"), nsmall = 0, big.mark = ","), "/", format(return_estimate(model.outputs, input$Place, "total-households", "Total-Households", model_horizon_year, "UrbanSim"), nsmall = 0, big.mark = ","))})
    output$Jobs <- renderText({paste("Jobs: ",  format(return_estimate(model.outputs, input$Place, "total-jobs", "Total-Jobs", model_base_year, "lehd/lodes"), nsmall = 0, big.mark = ","), "/", format(return_estimate(model.outputs, input$Place, "total-jobs", "Total-Jobs", model_horizon_year, "UrbanSim"), nsmall = 0, big.mark = ","))})
    output$HHSize <- renderText({paste("Household Size: ", round(return_estimate(model.outputs, input$Place, "total-population", "Total-Population", model_base_year, "acs/acs5")/return_estimate(model.outputs, input$Place, "total-households", "Total-Households", model_base_year, "acs/acs5"), 2), "/", round(return_estimate(model.outputs, input$Place, "total-population", "Total-Population", model_horizon_year, "UrbanSim")/return_estimate(model.outputs, input$Place, "total-households", "Total-Households", model_horizon_year, "UrbanSim"), 2))})    
    
    output$MedianAge <- renderText({paste("Average Age: ",  format(round(return_estimate(model.outputs, input$Place, "age", "Median Age", model_base_year, "acs/acs5"), 1), nsmall = 1, big.mark = ","), "/", format(round(return_estimate(model.outputs, input$Place, "age", "Median Age", model_horizon_year, "UrbanSim"), 1), nsmall = 1, big.mark = ","))})
    output$MedianIncome <- renderText({paste("Average Household Income: $",  format(round(return_estimate(model.outputs, input$Place, "household-income", "Median Income", model_base_year, "acs/acs5"),-2), nsmall = 0, big.mark = ","), "/ $", format(round(return_estimate(model.outputs, input$Place, "household-income", "Median Income", model_horizon_year, "UrbanSim"),-2), nsmall = 0, big.mark = ","))})
    
    output$AvgTT <- renderText({paste("Average Time to Work: ", format(round(return_estimate(model.outputs, input$Place, "total-commute-time", "Total Commute Time", model_base_year, "acs/acs5")/return_estimate(model.outputs, input$Place, "total-commute-trips", "Total Commute Trips", model_base_year, "acs/acs5"), 1), nsmall = 1, big.mark = ","), " minutes /", format(round(return_estimate(model.outputs, input$Place, "total-commute-time", "Total Commute Time", model_horizon_year, "SoundCast")/return_estimate(model.outputs, input$Place, "total-commute-trips", "Total Commute Trips", model_horizon_year, "SoundCast"), 1), nsmall = 1, big.mark = ",")," minutes")})    
    output$DriveAlone <- renderText({paste("Drive Alone Share: ",  format(round(return_share(model.outputs, input$Place, "commute-trips", "Drive-Alone", model_base_year, "acs/acs5")*100, 1), nsmall = 1, big.mark = ","), "% /", format(round(return_share(model.outputs, input$Place, "commute-trips", "Drive-Alone", model_horizon_year, "SoundCast")*100, 1), nsmall = 1, big.mark = ","),"%")})
    output$Transit <- renderText({paste("Transit Share: ",  format(round(return_share(model.outputs, input$Place, "commute-trips", "Transit", model_base_year, "acs/acs5")*100, 1), nsmall = 1, big.mark = ","), "% /", format(round(return_share(model.outputs, input$Place, "commute-trips", "Transit", model_horizon_year, "SoundCast")*100, 1), nsmall = 1, big.mark = ","),"%")})
    
    # Interactive Tables
    output$table_mode_to_work <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,modes_var_name,modes_var_descr,modes_cols,modes_order,modes_length,modes_name,input$estimate_choice,model_base_year)})
    output$table_age <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,age_var_name,age_var_descr,age_cols,age_order,age_length,age_name,input$estimate_choice,model_base_year)})
    output$table_occupation <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,jobs_var_name,jobs_var_descr,jobs_cols,jobs_order,jobs_length,jobs_name,input$estimate_choice,model_base_year)})
    output$table_gender <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,gender_var_name,gender_var_descr,gender_cols,gender_order,gender_length,gender_name,input$estimate_choice,model_base_year)})
    output$table_worker <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,worker_var_name,worker_var_descr,worker_cols,worker_order,worker_length,worker_name,input$estimate_choice,model_base_year)})
    output$table_student <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,student_var_name,student_var_descr,student_cols,student_order,student_length,student_name,input$estimate_choice,model_base_year)})
    output$table_hhsize <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,hhsize_var_name,hhsize_var_descr,hhsize_cols,hhsize_order,hhsize_length,hhsize_name,input$estimate_choice,model_base_year)})
    output$table_hhownership <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,hhownership_var_name,hhownership_var_descr,hhownership_cols,hhownership_order,hhownership_length,hhownership_name,input$estimate_choice,model_base_year)})
    output$table_hhtype <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,hhtype_var_name,hhtype_var_descr,hhtype_cols,hhtype_order,hhtype_length,hhtype_name,input$estimate_choice,model_base_year)})
    output$table_hhincome <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,hhincome_var_name,hhincome_var_descr,hhincome_cols,hhincome_order,hhincome_length,hhincome_name,input$estimate_choice,model_base_year)})
    output$table_work_traveltime <- DT::renderDataTable({create_summary_table(model.outputs,input$Place,input$TT_Mode,traveltime_var_descr,traveltime_cols,traveltime_order,traveltime_length,traveltime_name,input$estimate_choice,model_base_year)})
    
    # Interactive Charts
    output$chart_mode_to_work <- renderPlotly({create_bar_chart(model.outputs,input$Place,modes_var_name,modes_var_descr,modes_cols,modes_order,modes_name,input$estimate_choice,model_base_year,"Commute Trips","stack")})
    output$chart_age <- renderPlotly({create_bar_chart(model.outputs,input$Place,age_var_name,age_var_descr,age_cols,age_order,age_name,input$estimate_choice,model_base_year,"Household Population","stack")})
    output$chart_occupation <- renderPlotly({create_bar_chart(model.outputs,input$Place,jobs_var_name,jobs_var_descr,jobs_cols,jobs_order,jobs_name,input$estimate_choice,model_base_year,"Jobs","stack")})
    output$chart_gender <- renderPlot({create_pie_chart(model.outputs,input$Place,gender_var_name,gender_var_descr,gender_cols,gender_order,gender_name,input$estimate_choice,model_base_year,"Household Population")})
    output$chart_worker <- renderPlotly({create_bar_chart(model.outputs,input$Place,worker_var_name,worker_var_descr,worker_cols,worker_order,worker_name,input$estimate_choice,model_base_year,"Household Population","stack")})
    output$chart_student <- renderPlotly({create_bar_chart(model.outputs,input$Place,student_var_name,student_var_descr,student_cols,student_order,student_name,input$estimate_choice,model_base_year,"Household Population","stack")})
    output$chart_hhsize <- renderPlotly({create_bar_chart(model.outputs,input$Place,hhsize_var_name,hhsize_var_descr,hhsize_cols,hhsize_order,hhsize_name,input$estimate_choice,model_base_year,"Total Households","stack")})
    output$chart_hhownership <- renderPlotly({create_bar_chart(model.outputs,input$Place,hhownership_var_name,hhownership_var_descr,hhownership_cols,hhownership_order,hhownership_name,input$estimate_choice,model_base_year,"Total Households","stack")})
    output$chart_hhtype <- renderPlotly({create_bar_chart(model.outputs,input$Place,hhtype_var_name,hhtype_var_descr,hhtype_cols,hhtype_order,hhtype_name,input$estimate_choice,model_base_year,"Total Households","stack")})
    output$chart_hhincome <- renderPlotly({create_bar_chart(model.outputs,input$Place,hhincome_var_name,hhincome_var_descr,hhincome_cols,hhincome_order,hhincome_name,input$estimate_choice,model_base_year,"Total Households","stack")})
    output$chart_work_traveltime <- renderPlotly({create_bar_chart(model.outputs,input$Place,input$TT_Mode,traveltime_var_descr,traveltime_cols,traveltime_order,traveltime_name,input$estimate_choice,model_base_year,"Commute Trips","stack")})
    
    
    # Interactive Map
    #output$map_mode_to_work <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all",input$Mode_Var,"commute-trips","total-trips",input$Mode_Year , "Blues", paste0(input$Mode_Var ," to Work"), "", "%")})
    #output$map_occupation <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","jobs",input$Job_Var, input$Job_Year , "Blues", paste0(input$Job_Var ," jobs"), "", "%")})
    #output$map_age <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","age",input$Age_Var, input$Age_Year , "Blues", paste0("People ",input$Age_Var), "", "%")})
    #output$map_gender <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","gender",input$Gender_Var, input$Gender_Year , "Blues", paste0("People ",input$Gender_Var), "", "%")})
    #output$map_worker <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","worker",input$Worker_Var, input$Worker_Year , "Blues", paste0(input$Worker_Var), "", "%")})
    #output$map_student <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","student",input$Student_Var, input$Student_Year , "Blues", paste0(input$Student_Var), "", "%")})
    #output$map_hhsize <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-size",input$HHSize_Var, input$HHSize_Year , "Blues", paste0(input$HHSize_Var), "", "%")})
    #output$map_hhownership <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-ownership",input$HHOwn_Var, input$HHOwn_Year , "Blues", paste0(input$HHOwn_Var), "", "%")})
    #output$map_hhtype <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-type",input$HHType_Var, input$HHType_Year , "Blues", paste0(input$HHType_Var), "", "%")})
    #output$map_hhincome <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all","","household-income",input$HHIncome_Var, input$HHIncome_Year , "Blues", paste0(input$HHIncome_Var), "", "%")})
    #output$map_work_traveltime <- renderLeaflet({create_tract_map_pick_variable(model.outputs,input$Place,input$estimate_choice,"all",input$TT_Mode,"commute-time",input$TT_Var, input$TT_Year , "Blues", paste0(input$TT_Var), "", "%")})
    
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
