#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

#Dataset ile tablo olusturabilmek icin;

if (!require("DT")) install.packages('DT')
library(DT)

if("shinythemes" %in% rownames(installed.packages()) == FALSE) {install.packages("shinythemes")}
library(shinythemes)

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)

if (!require("fmsb")) install.packages('fmsb')
library(fmsb)

if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
library(devtools)

devtools::install_github("ricardo-bion/ggradar",dependencies=TRUE)
library(ggradar)


if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}

# dataset import; 
pokemon <- read.csv("C:/Users/Berkay ABAY/Downloads/Pokemon.csv")


ui <- fluidPage(theme = shinytheme("flatly"),


#   Navbar olusturma 
    navbarPage("Pokemon Dataset",
        
#       Navbar1 e ait elemanlar
        tabPanel("Stats",
    
                 
#   Navbar1 in sidebarPanel inde gorunecek elemanlar
    sidebarPanel(
        selectInput(inputId = "dataset", 
                    label = "Choose Data:",
                    choices = c("HP", "Attack", "Defense","Speed","Special Attack", "Special Defense")),
        
        radioButtons(inputId = "graph_type", label = "Select Graph Type:", c("Histogram", "Density Plot"))
                ),
        
#   Navbar1 in mainPanelinde gorunecek elemanlar
    mainPanel( 
        plotOutput("distPlot"),
        DT::dataTableOutput("mytable")
             )),
    
#   Navbar2 de gorunecek elemanlar
    tabPanel("Generation",
             sidebarPanel(
                 radioButtons(inputId = "dataset2", 
                              label = "Choose Graph:",
                              choices = c("Number of Pokemon per Generation",
                                          "Number of Legendary per Generation"))
                 
             ),
             mainPanel(
                 
            plotOutput("dispPlot_2"),
            DT::dataTableOutput("mytable2")
            
             )),
    
#   Navbar3 de gorunecek elemanlar
    tabPanel("Pokemons",
             sidebarPanel(
               selectInput(inputId = "pokemon_name", 
                           label   = "Choose Pokemon:",
                           choices = c("Select Pokemon" ,sort(pokemon$Name))),
               
               selectInput(inputId  = "pokemon_name2", 
                           label    = "Choose Pokemon:",
                           choices  = c("Select Pokemon" ,sort(pokemon$Name)))
             ),
             
             mainPanel(
               
               plotOutput("VersusradarChart"),
               DT::dataTableOutput("mytable3")
               
             )),

    tabPanel("Types",
             
           sidebarPanel( 
             selectInput(inputId = "dataset_Type", 
                       label     = "Choose Type:",
                       choices   = c("Primary Type", "Secondary Type")),
          
            radioButtons(inputId = "datasetGen", 
                        label    = "Select Generation:",
                        choices  = c("Generation1", "Generation2", "Generation3",
                                     "Generation4", "Generation5", "Generation6"))),
             mainPanel(
               
            plotOutput("GenTypeChart")
               
             )
    )
))


server <- function(input, output) {

    
################################################################################    
#### ---------------------# 1. Bolum icin degiskenler# ---------------------#### 
################################################################################   
    
#   Cizdirilecek Grafik tipinin belirlenmesi
    graphType <- reactive({
        switch(input$graph_type,
               "Histogram"          = "hist",
               "Density Plot"       = "density"  )
    })
    
#   Cizdirilecek verinin secilmesi
    datasetInput <- reactive({
        switch(input$dataset,
               "HP"                 = pokemon$HP,
               "Attack"             = pokemon$Attack,
               "Defense"            = pokemon$Defense,
               "Speed"              = pokemon$Speed,
               "Special Attack"     = pokemon$Sp..Atk,
               "Special Defense"    = pokemon$Sp..Def  )
    })
    
    
    
#   Tabloda x eksenindeki etiketin her degisken icin farkli gosterilmesi
    labelInput <- reactive({
        switch(input$dataset,
               "HP"                = "Hit Point",
               "Attack"            = "Attack",
               "Defense"           = "Defense",
               "Speed"             = "Speed",
              "Special Attack"     = "Special Attack",
              "Special Defense"    = "Special Defense"  )
    })
    
#   Her bir farkli ozelligi cizdirirken farkli renk kullanmak
    colourSelector <- reactive({
        switch(input$dataset,
               "HP"                 = "red",
               "Attack"             = "yellow",
               "Defense"            = "blue",
               "Speed"              = "green",
               "Special Attack"     = "goldenrod2",
               "Special Defense"    = "cyan4"  ) }
    )
    
    
################################################################################    
#### ---------------------# 2. Bolum icin degiskenler# ---------------------#### 
################################################################################   
    
   datasetInput2 <- reactive({
       switch(input$dataset2,
              "Number of Pokemon per Generation"          = "PokemonperGen",
              "Number of Legendary per Generation"        = "LegendaryperGen"  )
   })
  
    
################################################################################    
#### ---------------------# 3. Bolum icin degiskenler# ---------------------#### 
################################################################################  
    
    selectedData <- reactive({
      
      as.character(input$pokemon_name)
      
    })
    
    selectedData2 <- reactive({
      
      as.character(input$pokemon_name2)
      
    })
    
    
################################################################################    
#### ---------------------# 4. Bolum icin degiskenler# ---------------------#### 
################################################################################  
    
    Type_Value <- reactive({
      switch(input$dataset_Type,
             "Primary Type"   = "Type1",
             "Secondary Type" = "Type2"  )
    })
    
    Gen_Value <- reactive({
      switch(input$datasetGen,
             "Generation1"    = 1,
             "Generation2"    = 2,
             "Generation3"    = 3,
             "Generation4"    = 4,
             "Generation5"    = 5,
             "Generation6"    = 6 )}
    )
################################################################################    
#####   ---------------------# 1. Bolum icin Cizimler# ---------------------#### 
################################################################################   

#   Secilen Grafigin cizdirilmesi
    output$distPlot <- renderPlot({
        
        graphtyp      <- graphType()
        dataset_value <- datasetInput()
        labelx        <- labelInput()
        colour_value  <- colourSelector()
        graphtype2    <- datasetInput2()
        
        if(graphtyp == "hist")
        {
          
        ggplot(pokemon, aes(x=dataset_value)) + 
            geom_histogram(binwidth=4, fill=colour_value, colour="black") +
            labs(x=labelx, y="Frequency") + 
            theme_classic()
        }
        
        else if (graphtyp == "density")
          {
          
            ggplot(pokemon, aes(x=dataset_value, fill=Legendary)) + 
            geom_density(alpha=0.5) +labs(x=labelx, y="Frequency") + 
            theme_classic() 
        }
        
    })
    
  
    
#   Dataset gosterebilmek icin 
    output$mytable = DT::renderDataTable({
        
#       Secilen HP, Attack vb. degere gore tablo guncelleneceginden degisen deger ayri dataframe e alinirsa surekli guncellenir
        dfpokemons <- data.frame(Value = datasetInput())
        
#       Siralama icin
        orderedDataset <- order(datasetInput())
        
#       Cizdirilecek dataframe in olusturulmasi
         df_new <- data.frame(Name =  pokemon$Name[orderedDataset], 
                              Type1 = pokemon$Type.1[orderedDataset],
                              Type2 = pokemon$Type.2[orderedDataset],
                              Value = dfpokemons$Value[orderedDataset],
                              Legendary = pokemon$Legendary[orderedDataset]
         )
         
         df_new
        
        })
    
################################################################################    
#####   ---------------------# 2. Bolum icin Cizimler# ---------------------#### 
################################################################################  
  
    # Her Jenerasyonun agirligini bulmak icin.
    Generation_Level <-  as.factor(pokemon$Generation) 
    TotalGeneration1 <-  as.numeric(sum(Generation_Level == 1))
    TotalGeneration2 <-  as.numeric(sum(Generation_Level == 2))
    TotalGeneration3 <-  as.numeric(sum(Generation_Level == 3))
    TotalGeneration4 <-  as.numeric(sum(Generation_Level == 4))
    TotalGeneration5 <-  as.numeric(sum(Generation_Level == 5))
    TotalGeneration6 <-  as.numeric(sum(Generation_Level == 6))

  
    TotalGenerations <- c(TotalGeneration1, TotalGeneration2, 
                          TotalGeneration3, TotalGeneration4, 
                          TotalGeneration5, TotalGeneration6)
    
    TotalGenerations <- TotalGenerations/ 8

    
    generations_frame <- data.frame(Level = c("Generation1","Generation2",
                                              "Generation3","Generation4",
                                              "Generation5","Generation6"), Pokemon_Percentage = TotalGenerations )
                                    
    brand <- generations_frame$Level
    percent <- generations_frame$Pokemon_Percentage
    
    ypos <- cumsum(percent) - 0.5 * percent
    ypos <- 100- ypos                
    
    
    #   Secilen Grafigin cizdirilmesi
    output$dispPlot_2 <- renderPlot({

      if(datasetInput2() == "PokemonperGen")

      {
        
        ggplot()+theme_bw() + 
          geom_bar(aes(x = 2, y = percent, fill = brand),stat = "identity", color = "black") + 
          coord_polar("y", start = 0) + 
          ggtitle("Number of Pokemon per Generation[%]") + 
          theme(plot.title = element_text(hjust = 0.5, size = 20),
                axis.title = element_blank(), axis.text = element_blank(), 
                axis.ticks = element_blank(), panel.grid = element_blank(),
                panel.border = element_blank()) +
                guides(fill = guide_legend(reverse= TRUE)) +
                scale_fill_brewer(palette = "RdYlGn", name = "Gen.") +
                theme(legend.text = element_text(size = 15), 
                legend.title = element_text(hjust = 0.5, size = 18),
                legend.key.size = unit(1,"cm")) + 
                geom_text(aes(x = 2, y = ypos, label = paste0(percent,"%")), 
                color = "black", size = 4.5) + xlim(0.5, 2.5)
      }
      
      else if (datasetInput2() == "LegendaryperGen")
      {
        
        ggplot(pokemon) + geom_bar(aes(y = Generation, fill = Legendary)) + theme_light() +
          labs(x = "Number of Pokemon", y = "Generations", title  = "Number of Legendary Pokemon per Generation")
      }}) 
    
    output$mytable2 = DT::renderDataTable({
      
      gen1Legendarycount <- sum(pokemon$Generation == 1 & pokemon$Legendary == "True")
      gen2Legendarycount <- sum(pokemon$Generation == 2 & pokemon$Legendary == "True")
      gen3Legendarycount <- sum(pokemon$Generation == 3 & pokemon$Legendary == "True")
      gen4Legendarycount <- sum(pokemon$Generation == 4 & pokemon$Legendary == "True")
      gen5Legendarycount <- sum(pokemon$Generation == 5 & pokemon$Legendary == "True")
      gen6Legendarycount <- sum(pokemon$Generation == 6 & pokemon$Legendary == "True")
      
      gen1LegType1 = pokemon$Type.1[which(pokemon$Generation == 1 & pokemon$Legendary == "True")] 
      gen2LegType1 = pokemon$Type.1[which(pokemon$Generation == 2 & pokemon$Legendary == "True")] 
      gen3LegType1 = pokemon$Type.1[which(pokemon$Generation == 3 & pokemon$Legendary == "True")] 
      gen4LegType1 = pokemon$Type.1[which(pokemon$Generation == 4 & pokemon$Legendary == "True")] 
      gen5LegType1 = pokemon$Type.1[which(pokemon$Generation == 5 & pokemon$Legendary == "True")] 
      gen6LegType1 = pokemon$Type.1[which(pokemon$Generation == 6 & pokemon$Legendary == "True")] 
      
      TotalGenerationsVec <- c(TotalGeneration1, TotalGeneration2, 
                               TotalGeneration3, TotalGeneration4, 
                               TotalGeneration5, TotalGeneration6)

      genLegendaryCount = c(gen1Legendarycount, gen2Legendarycount, 
                            gen3Legendarycount, gen4Legendarycount, 
                            gen5Legendarycount, gen6Legendarycount)
      
      genNormalCount <- TotalGenerationsVec - genLegendaryCount
      
      df_new2 <- mutate(generations_frame, Normal_Count = genNormalCount, Legendary_Count = genLegendaryCount)
      
      df_new2
      
    })
    
    

################################################################################    
#### ---------------------# 3. Bolum icin Cizimler# ------------------------#### 
################################################################################
    
    max_min <- data.frame( HP = c(max(pokemon$HP), min(pokemon$HP)), 
                           Attack = c(max(pokemon$Attack), min(pokemon$Attack)),
                           Defense = c(max(pokemon$Defense), min(pokemon$Defense)), 
                           Speed = c(max(pokemon$Speed), min(pokemon$Speed)),
                           SpecialAttack = c(max(pokemon$Sp..Atk), min(pokemon$Sp..Atk)), 
                           SpecialDefense = c(max(pokemon$Sp..Def), min(pokemon$Sp..Def)))
    
    rownames(max_min) <- c("Maximal", "Minimal")

    
    
    output$VersusradarChart <- renderPlot({
      
      if(selectedData() != "Select Pokemon" & selectedData2() != "Select Pokemon"   )
      
        { 
        
        pokemon_ind  <- which(pokemon$Name == selectedData())
        pokemon_ind2 <- which(pokemon$Name == selectedData2())
        
        pokemon_stat <- data.frame(HP             = pokemon$HP[pokemon_ind],
                                   Attack         = pokemon$Attack[pokemon_ind],
                                   Defense        = pokemon$Defense[pokemon_ind],
                                   Speed          = pokemon$Speed[pokemon_ind],
                                   SpecialAttack  = pokemon$Sp..Atk[pokemon_ind],
                                   SpecialDefense = pokemon$Sp..Def[pokemon_ind] )
        
        pokemon_stat2 <- data.frame(HP            = pokemon$HP[pokemon_ind2],
                                   Attack         = pokemon$Attack[pokemon_ind2],
                                   Defense        = pokemon$Defense[pokemon_ind2],
                                   Speed          = pokemon$Speed[pokemon_ind2],
                                   SpecialAttack  = pokemon$Sp..Atk[pokemon_ind2],
                                   SpecialDefense = pokemon$Sp..Def[pokemon_ind2] )
        
        
        row.names(pokemon_stat)       =   c(pokemon$Name[pokemon_ind])
        row.names(pokemon_stat2)      =   c(pokemon$Name[pokemon_ind2])
        
        stats_dataFrame <- rbind(max_min, pokemon_stat, pokemon_stat2)
        

      }
      
      colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9)  )
      colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )
      
      tryCatch(
        expr = {
          
          radarchart(stats_dataFrame,
                     axistype = 1,
                     pcol = colors_border , pfcol = colors_in   , plwd = 2, plty = 1,
                     cgcol = "grey", cglty = 1, cglwd = 0.8, axislabcol = "black", 
                     title = "Pokemon Stats", vlcex = 0.8 )
        },
      
        error = function(e){ 
          "Please select Pokemon 1 and Pokemon 2."
        })
    })
    

    output$mytable3 = DT::renderDataTable({
      
      pokemon_ind  <- which(pokemon$Name == selectedData())
      pokemon_ind2 <- which(pokemon$Name == selectedData2())
      
      pokemon_stat <- data.frame(HP             = pokemon$HP[pokemon_ind],
                                 Attack         = pokemon$Attack[pokemon_ind],
                                 Defense        = pokemon$Defense[pokemon_ind],
                                 Speed          = pokemon$Speed[pokemon_ind],
                                 SpecialAttack  = pokemon$Sp..Atk[pokemon_ind],
                                 SpecialDefense = pokemon$Sp..Def[pokemon_ind] )
      
      pokemon_stat2 <- data.frame(HP             = pokemon$HP[pokemon_ind2],
                                  Attack         = pokemon$Attack[pokemon_ind2],
                                  Defense        = pokemon$Defense[pokemon_ind2],
                                  Speed          = pokemon$Speed[pokemon_ind2],
                                  SpecialAttack  = pokemon$Sp..Atk[pokemon_ind2],
                                  SpecialDefense = pokemon$Sp..Def[pokemon_ind2] )
      
      
      row.names(pokemon_stat)   =   c(pokemon$Name[pokemon_ind])
      row.names(pokemon_stat2)  =   c(pokemon$Name[pokemon_ind2])
      
      # Karsilastirilan pokemonlarin dataset olarak rchart altinda gorunmesi icin
      stf_dataframe <- rbind(pokemon_stat, pokemon_stat2)
      stf_dataframe
      
    })
 
################################################################################    
#### ---------------------# 4. Bolum icin Cizimler# ------------------------#### 
################################################################################
    output$GenTypeChart <- renderPlot({

      ind_notempty <- which(pokemon$Type.2 != "")
      
      Type2Name <- unique(pokemon$Type.2[ind_notempty])
      Type1Name <- unique(pokemon$Type.1)
     

      # Type2Name <- unique(pokemon$Type.2)  
      
    if(Type_Value() == "Type1")
      
    {
      
      Gen_bug       <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Bug")
      Gen_dark      <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Dark")
      Gen_dragon    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Dragon")
      Gen_electric  <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Electric")
      Gen_Fairy     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Fairy")
      Gen_Fighting  <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Fighting")
      Gen_Fire      <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Fire")
      Gen_Flying    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Flying")
      Gen_Ghost     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Ghost")
      Gen_Grass     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Grass")
      Gen_Ice       <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Ice")
      Gen_Normal    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Normal")
      Gen_Poison    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Poison")
      Gen_Psychic   <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Psychic")
      Gen_Rock      <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Rock")
      Gen_Steel     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Steel")
      Gen_Water     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Water")
      Gen_Ground    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.1 == "Ground")
      
      Gen_Type <- c(Gen_Grass, Gen_Fire, Gen_Water, Gen_bug,
                    Gen_Normal, Gen_Poison, Gen_electric,
                    Gen_Ground, Gen_Fairy, Gen_Fighting, 
                    Gen_Psychic, Gen_Rock, Gen_Ghost, Gen_Ice,
                    Gen_dragon, Gen_dark, Gen_Steel, Gen_Flying)
      
      GenType <- t(rbind(Gen_Type, Type1Name))
      dat_GenType <- data.frame(TypeName = GenType[,2], cValue = as.numeric(GenType[,1]))
      
    }
    
     else if(Type_Value() == "Type2")
     {
      
           Gen_bug       <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Bug")
           Gen_dark      <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Dark")
           Gen_dragon    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Dragon")
           Gen_electric  <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Electric")
           Gen_Fairy     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Fairy")
           Gen_Fighting  <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Fighting")
           Gen_Fire      <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Fire")
           Gen_Flying    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Flying")
           Gen_Ghost     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Ghost")
           Gen_Grass     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Grass")
           Gen_Ice       <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Ice")
           Gen_Normal    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Normal")
           Gen_Poison    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Poison")
           Gen_Psychic   <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Psychic")
           Gen_Rock      <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Rock")
           Gen_Steel     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Steel")
           Gen_Water     <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Water")
           Gen_Ground    <- sum(pokemon$Generation == as.numeric(Gen_Value()) & pokemon$Type.2 == "Ground")
           
           Gen_Type <- c(Gen_Poison, Gen_Flying, Gen_dragon,
                         Gen_Ground, Gen_Fairy, Gen_Grass,Gen_Fighting,
                         Gen_Psychic, Gen_Steel, Gen_Ice, Gen_Rock,
                         Gen_dark, Gen_Water, Gen_electric, Gen_Fire,
                         Gen_Ghost, Gen_bug, Gen_Normal)
           
           GenType <- t(rbind(Gen_Type, Type2Name))
           dat_GenType <- data.frame(TypeName = GenType[,2], cValue = as.numeric(GenType[,1]))
         }
       
           ggplot(dat_GenType, aes(x=TypeName, y=cValue)) + 
             geom_bar(stat = "identity")      
    
})
    
     
}
 

# Run the application 
shinyApp(ui = ui, server = server)
