# zzz_model_plots.R
# ::rtemis::
# 2026- EDG rtemis.org

#' @importFrom rtemis.draw confusion_input
#' @include plot.SupervisedSession.R plot_learning.R plot_varimp.R plot_metric.R
#' @include present.R plot_massglm.R plot_true_pred.R plot_confusion.R plot_roc.R
NULL

# %% Model plotting methods ----
method(plot, SupervisedSession) <- draw_supervised_session
method(plot_session, Supervised) <- draw_object_session
method(plot_session, SupervisedRes) <- draw_object_session
method(plot_learning, Supervised) <- draw_model_learning_curve
method(varimp_plot_data, Supervised) <- extract_varimp_plot_data
method(varimp_plot_data, SupervisedRes) <- extract_varimp_plot_data
method(plot_varimp, Supervised) <- draw_model_varimp
method(plot_varimp, SupervisedRes) <- draw_model_varimp
method(metric_plot_data, SupervisedRes) <- extract_metric_plot_data
method(plot_metric, SupervisedRes) <- draw_model_metric
method(present, Regression) <- present_regression
method(present, Classification) <- present_classification
method(present, SupervisedRes) <- present_resampled
method(massglm_plot_data, MassGLM) <- extract_massglm_plot_data
method(plot_manhattan, MassGLM) <- draw_massglm_manhattan
method(plot, MassGLM) <- draw_massglm_volcano
method(regression_plot_data, Regression) <- extract_regression_plot_data
method(regression_plot_data, RegressionRes) <- extract_regression_plot_data
method(plot_true_pred, Regression) <- draw_regression_predictions
method(plot_true_pred, RegressionRes) <- draw_regression_predictions
method(
  confusion_input,
  ClassificationMetrics
) <- extract_confusion_metrics
method(
  confusion_input,
  ClassificationMetricsRes
) <- extract_confusion_metrics
method(
  classification_plot_data,
  Classification
) <- extract_classification_plot_data
method(
  classification_plot_data,
  ClassificationRes
) <- extract_classification_plot_data
method(plot_true_pred, Classification) <- draw_classification_predictions
method(plot_true_pred, ClassificationRes) <- draw_classification_predictions
method(roc_plot_data, Classification) <- extract_roc_plot_data
method(roc_plot_data, ClassificationRes) <- extract_roc_plot_data
method(plot_roc, Classification) <- draw_model_roc
method(plot_roc, ClassificationRes) <- draw_model_roc
