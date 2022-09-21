library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(ROCR)
library(pROC)
library(rpart)
library(rpart.plot)
library(nnet)
library(NeuralNetTools)
library(kableExtra)
library(knitr)
library(devtools)


data1 <- read_excel("C:/Users/amirk/OneDrive/Bureau/german_credit_dataset_Projet.xlsx")
data1[which(data1$Statut==2),16]=0
data1
data2 <- read_excel("C:/Users/amirk/OneDrive/Bureau/ROJ.xlsx")
data2[which(data2$V16==2),16]=0
attach(data2)
a<-c("Solde du compte courant",	"Historique des credits","Motif","Compte d'epargne / obligations"	,"Employe depuis","Statut personnel et sexe",	"Autres debiteurs / garants",		"Residence actuelle depuis",		"Logement", 	"Nombre de credits existants dans cette banqueg",		"Emploi"	,	"Nombre de personnes à charge",	"Statut")
b<-c("Duree de credit (en mois)","Montant du credit","Age")


#les models pour Comparaison:
train_id=sample(1000,750)
data_train=data2[train_id,]
data_test=data2[-train_id,]
logit=glm(V16~.,data = data_train,family = binomial)
prob_test=predict(logit,data_test,type = "response")
pred_test=prediction(prob_test,data_test$V16)
perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")
m1_1_AUROC <- round(performance(pred_test, measure = "auc")@y.values[[1]]*100, 2)
m1_1_KS <- round(max(attr(perf_roc_test,'y.values')[[1]]-attr(perf_roc_test,'x.values')[[1]])*100, 2)
m1_1_Gini <- (2*m1_1_AUROC - 100)


mycontrol = rpart.control(cp = 0, xval = 10)
model_ar <- rpart(V16~ .,method = "class" ,control = mycontrol, data=data_train)
prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
pred_test_ar=prediction(prob_test_ar,data_test$V16)
perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")
m2_2_AUROC <- round(performance(pred_test_ar, measure = "auc")@y.values[[1]]*100, 2)
m2_2_KS <- round(max(attr(perf_roc_test_ar,'y.values')[[1]]-attr(perf_roc_test_ar,'x.values')[[1]])*100, 2)
m2_2_Gini <- (2*m2_2_AUROC - 100)


Neural<- nnet(V16~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
fitNeural <- predict(Neural,newdata=data_test)
prednn = prediction( fitNeural, data_test$V16)
perfnn <- performance(prednn, "tpr", "fpr")
m3_3_AUROC <- round(performance(prednn, measure = "auc")@y.values[[1]]*100, 2)
m3_3_KS <- round(max(attr(perfnn,'y.values')[[1]]-attr(perfnn,'x.values')[[1]])*100, 2)
m3_3_Gini <- (2*m3_3_AUROC - 100)









shinyApp(
  ui = tagList(
    navbarPage(theme = shinytheme("cerulean"),
               "German Credit",
               navbarMenu("Analyse univariee",
                          
                          tabPanel("Variables Quantitatives", 
                                   sidebarPanel(
                                     selectInput("quanti","Choisir une variable Quantitative",
                                                 choices=c(colnames(data1[c(2,5,11)]))),
                                     h3("Resume Statistique :"),
                                     textOutput("statdes"),
                                     h4("Box plot : "),
                                     plotlyOutput("plot2")
                                     
                                   ),
                                   mainPanel(
                                     
                                     plotlyOutput("p1")
                                     
                                   )
                          ),
                          
                          tabPanel("Variables Qualitatives",
                                   sidebarPanel(
                                     selectInput("quali","Choisir une variable Qualitative",choices=c(colnames(data1[-c(2,5,11)])))
                                   ),
                                   mainPanel(
                                     plotlyOutput("plot1"),
                                     tableOutput("t1")
                                     
                                   )
                          )
               ),
               
               navbarMenu("Analyse bivariee",
                          tabPanel("Statut ~ var.quantitative",
                                   sidebarPanel(
                                     selectInput("bi1","Choisir une variable Quantitative",
                                                 choices =c(colnames(data1[c(2,5,11)])) ),
                                     h3("Test de Student :"),
                                     tableOutput("test")
                                   ),
                                   mainPanel(
                                     plotlyOutput("biplot1")
                                   )
                                   
                          ),
                          
                          tabPanel("Statut~ var.qualitative",
                                   sidebarPanel(
                                     selectInput("bi2","Choisir une variable Qualitative",
                                                 choices =c(colnames(data1[-c(2,5,11)]))),
                                     h3("Test du Khi2 :"),
                                     tableOutput("test1")
                                   ),
                                   mainPanel(
                                     plotOutput("biplot2")
                                   )
                          ),
                          tabPanel("Matrice de correlation",
                                   mainPanel(
                                     box(title = "Matrice de correlation",width = 700,height = 600,status = "primary", solidHeader = TRUE,background = "red",plotOutput("cor",width = 700,height = 700))
                                   )
                          )         
                          
               ),
               
               
               navbarMenu("Prevision",
                          
                          tabPanel("Logit",
                                   mainPanel(
                                     tabBox(type="pills",
                                            tabPanel(title ="ROC : Regression logistique",width = 900,height = 600,plotOutput("plot_Logit",width = 900,height = 500)),
                                            tabPanel(title = "Resume",tableOutput("resum"))
                                     )),
                                   sidebarPanel(sliderInput("sd1",label = "Choisir la taille du base du train",
                                                            min = 0,max = 1000,value = 750))
                          ),
                          tabPanel("Arbre de decision",
                                   mainPanel(
                                     tabBox(type="pills",tabPanel(title ="ROC: Arbre de decision",width = 900,height = 600,plotOutput("plot_ar",width = 900,height = 500)),
                                            tabPanel(plotOutput("arbre",width = 1100,height = 700),width = 1100,height = 700,title = "PLOT")
                                     )),
                                   sidebarPanel(sliderInput("sd2",label = "Choisir la taille de la base pour le train",
                                                            min = 0,max = 1000,value = 750))
                          ),
                          tabPanel("Reseau de neurones",
                                   mainPanel(
                                     tabBox(type="pills",tabPanel(title ="ROC: Reseau de neurones",width = 900,height = 600,plotOutput("plot_rn",width = 900,height = 500)),
                                            tabPanel(title = "Plot",plotOutput("plotnn",width = 1200,height = 600))
                                     )),
                                   sidebarPanel(sliderInput("sd3",label = "Choisir la taille de la base pour le train",
                                                            min = 0,max = 1000,value = 750))
                          )
                          
               ),
               
               tabPanel("Comparaison entre les methodes de prevision",
                        mainPanel(title="Comparaison",
                                  plotOutput("com"),
                                  tableOutput("tabc")
                        ),
                        sidebarLayout(
                          sidebarPanel("Indication",
                                       verbatimTextOutput("ind")
                          ),
                          sidebarPanel("Interpretation",
                                       verbatimTextOutput("inter")
                          )
                        )
                        
               )
               
               
               
               
    )
  )
  ,
  
  
  
  
  
  
  
  
  server = function(input, output) {
    
    output$plot1<-renderPlotly({
      plot_ly(
        y = c(table(data1[,input$quali])),
        type = "bar",
        marker = list(color = "green")
      )%>%layout(title = names(data1[,input$quali]))
    })
    
    output$t1<-renderTable({
      unlist(table(data1[,input$quali],dnn =names(data1[,input$quali]) ))
    },striped = TRUE,bordered = TRUE,spacing = "xs")
    
    output$plot2<-renderPlotly({
      x = unlist(data1[,input$quanti])
      p <- plot_ly(y = ~x, type = "box",line = list(color = "green"))%>%layout(title = names(data1[,input$quanti]))
    })
    
    output$statdes=renderText({
      summary(data1[,input$quanti])
    })
    
    output$p1<-renderPlotly({
      x = unlist(data1[,input$quanti])
      fit <- density(x)
      plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
        add_trace(x = fit$x, y = fit$y,type='scatter' ,mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
        layout(title=names(data1[,input$quanti]),yaxis2 = list(overlaying = "y", side = "right"))
    })
    
    
    output$biplot1<-renderPlotly({
      x1 <- unlist(data1[,input$bi1])
      plot_ly(y = ~x1, x = ~data1$Statut, type = "box",line = list(color = "#FF7F24"))%>%layout(title = names(data1[,input$bi1]),xaxis = list(title = 'Statut'))
    })
    
    output$test<-renderTable({
      x1 <- unlist(data1[,input$bi1])
      unlist(t.test(x1,data1$Statut))
    },striped = TRUE,bordered = TRUE,rownames = TRUE,spacing = "xs")
    
    output$test1<-renderTable({
      x1 <- unlist(data1[,input$bi2])
      unlist(chisq.test(x1,data1$Statut)[c(1,2,3,4)])
    },striped = TRUE,bordered = TRUE,rownames = TRUE,spacing = "s")
    
    output$biplot2<-renderPlot({
      x2 <- unlist(data1[,input$bi2])
      ggplot(data1, aes(factor(data1$Statut), fill = factor(x2))) +geom_bar(position = "dodge")+xlab("Statut")+ylab("Count") +
        ggtitle(names(data1[,input$bi2]))
    })
    
    output$cor<-renderPlot({cor1<-cor(data2)
    corrplot(cor1, type="upper", order="hclust", tl.col="black", tl.srt=45)})
    
    
    ##Model Logit:
    output$plot_Logit<-renderPlot({
      
      train_id=sample(1000,input$sd1)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      
      ##model Logit:
      logit=glm(V16~.,data = data_train,family = binomial)
      
      prob_train=predict(logit,data_train,type = "response")
      pred_train=prediction(prob_train,data_train$V16)
      perf_roc_train=performance(pred_train,measure = "tpr",x.measure ="fpr")
      auc_train=performance(pred_train,measure="auc")
      
      prob_test=predict(logit,data_test,type = "response")
      pred_test=prediction(prob_test,data_test$V16)
      perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")
      auc_test=performance(pred_test,measure="auc")
      #plot:
      plot(perf_roc_train,col="red",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      par(new=TRUE)
      plot(perf_roc_test,col="green",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      precision=abs(auc_test@y.values[[1]]/auc_train@y.values[[1]])*100
      text(x=0.9,y=0.3,paste("Precision = ",round(precision,digits = 3)),col ="blue")
      text(x=0.9,y=0.2,paste("auc(train)=",round(auc_train@y.values[[1]],digits = 3)),col ="red")
      text(x=0.9,y=0.1,paste("auc(test)=",round(auc_test@y.values[[1]],digits = 3)),col ="green")
    })
    
    output$resum<-renderTable({
      train_id=sample(1000,input$sd1)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      logit=glm(V16~.,data = data_train,family = binomial)
      summary(logit)$coefficients
    },striped = TRUE,bordered = TRUE,rownames = TRUE,spacing = "xs")
    
    #####Plot Arbre:
    output$plot_ar<-renderPlot({
      
      train_id=sample(1000,input$sd2)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      ##Decision tree:
      mycontrol = rpart.control(cp = 0, xval = 10)
      model_ar <- rpart(V16~ .,method = "class" ,control = mycontrol, data=data_train)
      prob_train_ar=predict(model_ar,data_train,type = "prob")[,2]
      pred_train_ar=prediction(prob_train_ar,data_train$V16)
      perf_roc_train_ar=performance(pred_train_ar,measure = "tpr",x.measure ="fpr")
      auc_train_ar=performance(pred_train_ar,measure="auc")
      
      prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
      pred_test_ar=prediction(prob_test_ar,data_test$V16)
      perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")
      auc_test_ar=performance(pred_test_ar,measure="auc")
      
      #plot:
      plot(perf_roc_train_ar,col="red",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      par(new=TRUE)
      plot(perf_roc_test_ar,col="green",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      precision_ar=abs(auc_test_ar@y.values[[1]]/auc_train_ar@y.values[[1]])*100
      
      text(x=0.9,y=0.3,paste("Precision = ",round(precision_ar,digits = 3)),col ="blue")
      text(x=0.9,y=0.2,paste("auc(train)=",round(auc_train_ar@y.values[[1]],digits = 3)),col ="red")
      text(x=0.9,y=0.1,paste("auc(test)=",round(auc_test_ar@y.values[[1]],digits = 3)),col ="green")})
    
    output$arbre<-renderPlot({
      train_id=sample(1000,input$sd3)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      ##Decision tree:
      mycontrol = rpart.control(cp = 0, xval = 10)
      model_ar <- rpart(V16~ .,method = "class" ,control = mycontrol, data=data_train)
      #plot tree
      prp(model_ar,type=2,extra=1)})
    
    output$tab1<-renderText({
      train_id=sample(1000,input$sd2)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      mycontrol = rpart.control(cp = 0, xval = 10)
      model_ar <- rpart(V16~ .,method = "class" ,control = mycontrol, data=data_train)
      summary(model_ar)})
    
    ##### RN :
    
    output$plot_rn<-renderPlot({
      train_id=sample(1000,input$sd3)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      
      Neural<- nnet(V16~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
      
      fitNeural_train <- predict(Neural,
                                 newdata=data_train)
      prednn_train = prediction( fitNeural_train, data_train$V16)
      perfnn_train <- performance(prednn_train, "tpr", "fpr")
      auc_train_nn=performance(prednn_train,measure="auc")
      auc_train_nn
      
      
      fitNeural <- predict(Neural,
                           newdata=data_test)
      prednn = prediction( fitNeural, data_test$V16)
      perfnn <- performance(prednn, "tpr", "fpr")
      auc_test_nn=performance(prednn,measure="auc")
      auc_test_nn
      
      plot(perfnn_train,col="red",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      par(new=TRUE)
      plot(perfnn,col="green",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      
      text(x=0.9,y=0.2,paste("auc(train)=",round(auc_train_nn@y.values[[1]],digits = 3)),col ="red")
      text(x=0.9,y=0.1,paste("auc(test)=",round(auc_test_nn@y.values[[1]],digits = 3)),col ="green")
      
      
    })
    
    output$plotnn<-renderPlot({
      train_id=sample(1000,input$sd3)
      data_train=data2[train_id,]
      data_test=data2[-train_id,]
      Neural<- nnet(V16~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
      plot.nnet(Neural,col = "red")
    })
    
    
    ## Comparaison :
    output$com<-renderPlot({
      
      plot(perf_roc_test, col='blue',lwd=2,  main='ROCs: Model Performance : Comparaison') # logistic regression
      plot(perf_roc_test_ar, col='green2',lwd=2, add=TRUE); # simple tree
      plot(perfnn, col='red',lwd=2, add=TRUE); # RN
      legend(0.6,0.5,
             c('m1:logistic reg','m2: Arbre de decision', 
               'm3: Reseau de Nerones'),
             col=c('blue','green2', 'red'),
             lwd=3)
      lines(c(0,1),c(0,1),col = "black", lty = 2 )
    })
    
    output$ind=renderText({
      "* AUC = Surface sous la courbe ROC.
* KS = Distance maximale entre les distributions de deux classes.
* Gini = 2*AUROCC - 1"
    })
    
    output$inter=renderText({
      "Le modele le plus adequat est le modele qui  correspont a l'AUC (ou KS ou Gini) la plus   elevee et la courbe de ROC en dessus des autres ROCs"
    })
    
    
    output$tabc=renderTable({
      models <- c('m1: Regression Logistique', 'm2: Arbre de decision',
                  'm3: Reseau de neurones')
      models_AUC <- c(m1_1_AUROC, m2_2_AUROC, m3_3_AUROC)
      models_KS <- c(m1_1_KS,m2_2_KS,m3_3_KS)
      models_Gini <- c(m1_1_Gini,m2_2_Gini,m3_3_Gini)
      model_performance_metric <- as.data.frame(cbind(models, models_AUC, models_KS, models_Gini))
      colnames(model_performance_metric) <- c("Modele", "AUC", "KS", "Gini")
      model_performance_metric
      
    },striped = TRUE,bordered = TRUE,rownames = TRUE,spacing = "s")
    
  }
)

