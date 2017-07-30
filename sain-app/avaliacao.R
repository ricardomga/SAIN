source("regressao_logistica.R")
source("mlpe.R")
source("mlp.R")
source("random_forest.R")
source("ksvm.R")
source("naive.R")
source("decision_tree.R")
source("tabu_search.R")
source("genalg.R")
source("monte_carlo.R")
source("sann.R")

resultados <- c(
    glm_desbalanceados(d),
    glm_smote(d),
    glm_oversampling(d),
    glm_undersampling(d),
    glm_rolling_desbalanceados(d),
    glm_rolling_smote(d),
    glm_rolling_oversampling(d),
    glm_rolling_undersampling(d),
    mlpe_desbalanceados(d),
    mlpe_smote(d),
    mlpe_oversampling(d),
    mlpe_undersampling(d),
    mlpe_rolling_desbalanceados(d),
    mlpe_rolling_smote(d),
    mlpe_rolling_oversampling(d),
    mlpe_rolling_undersampling(d),
    mlp_desbalanceados(d),
    mlp_smote(d),
    mlp_oversampling(d),
    mlp_undersampling(d),
    mlp_rolling_desbalanceados(d),
    mlp_rolling_smote(d),
    mlp_rolling_oversampling(d),
    mlp_rolling_undersampling(d),
    random_forest_desbalanceados(d),
    random_forest_smote(d),
    random_forest_oversampling(d),
    random_forest_undersampling(d),
    random_forest_rolling_desbalanceados(d),
    random_forest_rolling_smote(d),
    random_forest_rolling_oversampling(d),
    random_forest_rolling_undersampling(d),
    ksvm_desbalanceados(d),
    ksvm_smote(d),
    ksvm_oversampling(d),
    ksvm_undersampling(d),
    ksvm_rolling_desbalanceados(d),
    ksvm_rolling_smote(d),
    ksvm_rolling_oversampling(d),
    ksvm_rolling_undersampling(d),
    naive_bayes_desbalanceados(d),
    naive_bayes_smote(d),
    naive_bayes_oversampling(d),
    naive_bayes_undersampling(d),
    naive_bayes_rolling_desbalanceados(d),
    naive_bayes_rolling_smote(d),
    naive_bayes_rolling_oversampling(d),
    naive_bayes_rolling_undersampling(d),
    decision_tree_desbalanceados(d),
    decision_tree_smote(d),
    decision_tree_oversampling(d),
    decision_tree_undersampling(d),
    decision_tree_rolling_desbalanceados(d),
    decision_tree_rolling_smote(d),
    decision_tree_rolling_oversampling(d),
    decision_tree_rolling_undersampling(d))



modelos <- c(
    "glm_desbalanceados",
    "glm_smote",
    "glm_oversampling",
    "glm_undersampling",
    "glm_rolling_desbalanceados",
    "glm_rolling_smote",
    "glm_rolling_oversampling",
    "glm_rolling_undersampling",
    "mlpe_desbalanceados",
    "mlpe_smote",
    "mlpe_oversampling",
    "mlpe_undersampling",
    "mlpe_rolling_desbalanceados",
    "mlpe_rolling_smote",
    "mlpe_rolling_oversampling",
    "mlpe_rolling_undersampling",
    "mlp_desbalanceados",
    "mlp_smote",
    "mlp_oversampling",
    "mlp_undersampling",
    "mlp_rolling_desbalanceados",
    "mlp_rolling_smote",
    "mlp_rolling_oversampling",
    "mlp_rolling_undersampling",
    "random_forest_desbalanceados",
    "random_forest_smote",
    "random_forest_oversampling",
    "random_forest_undersampling",
    "random_forest_rolling_desbalanceados",
    "random_forest_rolling_smote",
    "random_forest_rolling_oversampling",
    "random_forest_rolling_undersampling",
    "ksvm_desbalanceados",
    "ksvm_smote",
    "ksvm_oversampling",
    "ksvm_undersampling",
    "ksvm_rolling_desbalanceados",
    "ksvm_rolling_smote",
    "ksvm_rolling_oversampling",
    "ksvm_rolling_undersampling",
    "naive_bayes_desbalanceados",
    "naive_bayes_smote",
    "naive_bayes_oversampling",
    "naive_bayes_undersampling",
    "naive_bayes_rolling_desbalanceados",
    "naive_bayes_rolling_smote",
    "naive_bayes_rolling_oversampling",
    "naive_bayes_rolling_undersampling",
    "decision_tree_desbalanceados",
    "decision_tree_smote",
    "decision_tree_oversampling",
    "decision_tree_undersampling",
    "decision_tree_rolling_desbalanceados",
    "decision_tree_rolling_smote",
    "decision_tree_rolling_oversampling",
    "decision_tree_rolling_undersampling")

metricas <- c(
    "ACC",
    "PRECISION1",
    "PRECISION2",
    "CE",
    "TNR1",
    "TNR2",
    "TPR1",
    "AUC")

matriz_resultados <- matrix(
    resultados,
    nrow = 56,
    ncol = 8,
    dimnames = list(modelos, metricas),
    byrow = TRUE)

View(matriz_resultados[order(-matriz_resultados[, 8]), ])

saveRDS(matriz_resultados[order(-matriz_resultados[, 8]), ], "resultados.Rda")


resultados_otimizacao_normal <- c(
  tabu_search_repair(10),
  genalg_dp(10),
  genalg_repair(10),
  montecarlo_dp(10),
  montecarlo_repair(10),
  sann_dp(10),
  sann_repair(10)
)

resultados_otimizacao_restricao <- c(
  tabu_search_repair_restricao(10),
  genalg_dp_restricao(10),
  genalg_repair_restricao(10),
  montecarlo_dp_restricao(10),
  montecarlo_repair_restricao(10),
  sann_dp_restricao(10),
  sann_repair_restricao(10)
  )

names(resultados_otimizacao_normal) <- c(
  "solucao tabu_search_repair",
  "lucro tabu_search_repair",
  "solucao genalg_dp",
  "lucro genalg_dp",
  "solucao genalg_repair",
  "lucro genalg_repair",
  "solucao montecarlo_dp",
  "lucro montecarlo_dp",
  "solucao montecarlo_repair",
  "lucro montecarlo_repair",
  "solucao sann_dp",
  "lucro sann_dp",
  "solucao sann_repair",
  "lucro sann_repair"
  )

names(resultados_otimizacao_restricao) <- c(
  "solucao tabu_search_repair_restricao",
  "lucro tabu_search_repair_restricao",
  "solucao genalg_dp_restricao",
  "lucro genalg_dp_restricao",
  "solucao genalg_repair_restricao",
  "lucro genalg_repair_restricao",
  "solucao montecarlo_dp_restricao",
  "lucro montecarlo_dp_restricao",
  "solucao montecarlo_repair_restricao",
  "lucro montecarlo_repair_restricao",
  "solucao sann_dp_restricao",
  "lucro sann_dp_restricao",
  "solucao sann_repair_restricao",
  "lucro sann_repair_restricao"
)

resultados_otimizacao_normal

resultados_otimizacao_restricao
