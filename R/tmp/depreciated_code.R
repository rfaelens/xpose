# Create pretty labels for facetting --------------------------------------
# Not needed in ggplot2 v2.0
# if (!is.null(by)) {
#   data[,by] <- lapply(X = by, FUN = function(x) {
#     factor(data[,x], labels = paste0(tolower(x),': ', levels(as.factor(data[,x]))))
#   })
# }


# Old xpose_geoms ---------------------------------------------------------
# library(ggxpose)
# xpdb    <- xpose_data(dir = 'inst/models/', runno = '037')
#
# xp_geoms(mapping = NULL, group = 'ID', xp_theme = xpdb$xp_theme,
#         name = 'abline', ggfun = 'geom_abline',
#         abline_slope = 2, abline_color = 'red', abline_size = 1,
#         line_color = 'blue', line_size = 999, line_linetype = 'dashed',
#         point_alpha = 2)
#
# arg <- update_args(arg, name,
#             text_alpha =99, text_size = 999, text_color = 'pink', text_label = 'hello world',
#             abline_color = 'red', abline_size = 1,line_color = 'blue', line_size = 999, line_linetype = 'dashed', point_alpha = 2)
#
# xp_annotate_text(xp_theme = xpdb$xp_theme, name ='text', x = 1, y =2, text_alpha =2, text_size = 999, text_color = 'pink', label = 'hello world')


# Older xpose_geoms -------------------------------------------------------
# xp_geoms <- function(mapping = NULL, xp_theme, group = NULL, name, ggfun, ...) {
#
#   if(!is.null(mapping)) {
#     mapping <- parse_arg(mapping, name)
#   }
#
#   # Make it more general and grep all stuff associated with a name then parse
#
#   if(ggfun %in% c('geom_line', 'geom_abline')) {
#     arg <- list(alpha    = xp_theme[[paste0(name, '_alpha')]],
#                 colour   = xp_theme[[paste0(name, '_color')]],
#                 linetype = xp_theme[[paste0(name, '_linetype')]],
#                 size     = xp_theme[[paste0(name, '_size')]])
#   }
#
#   if(ggfun == 'annotate') {
#     arg <- list(alpha      = xp_theme[[paste0(name, '_alpha')]],
#                 angle      = xp_theme[[paste0(name, '_angle')]],
#                 colour     = xp_theme[[paste0(name, '_color')]],
#                 family     = xp_theme[[paste0(name, '_family')]],
#                 fontface   = xp_theme[[paste0(name, '_fontface')]],
#                 hjust      = xp_theme[[paste0(name, '_hjust')]],
#                 lineheight = xp_theme[[paste0(name, '_lineheight')]],
#                 size       = xp_theme[[paste0(name, '_size')]],
#                 vjust      = xp_theme[[paste0(name, '_vjust')]])
#   }
#
#   if(ggfun == 'geom_point') {
#     arg <- list(alpha    = xp_theme[[paste0(name, '_alpha')]],
#                 colour   = xp_theme[[paste0(name, '_color')]],
#                 fill     = xp_theme[[paste0(name, '_fill')]],
#                 shape    = xp_theme[[paste0(name, '_shape')]],
#                 size     = xp_theme[[paste0(name, '_size')]],
#                 stroke   = xp_theme[[paste0(name, '_stroke')]])
#   }
#
#   if(ggfun == 'geom_smooth') {
#     arg <- list(alpha    = xp_theme[[paste0(name, '_alpha')]],
#                 colour   = xp_theme[[paste0(name, '_color')]],
#                 fill     = xp_theme[[paste0(name, '_fill')]],
#                 linetype = xp_theme[[paste0(name, '_linetype')]],
#                 method   = xp_theme[[paste0(name, '_method')]],
#                 se       = xp_theme[[paste0(name, '_se')]],
#                 shape    = xp_theme[[paste0(name, '_shape')]],
#                 size     = xp_theme[[paste0(name, '_size')]],
#                 stroke   = xp_theme[[paste0(name, '_stroke')]],
#                 weight   = xp_theme[[paste0(name, '_weight')]]#,
#                 #stat     = "smooth",
#                 #position = "identity"
#     )
#   }
#
#   arg <- update_args(arg, name, ...)
#   out <- xp_map(arg, mapping, ggfun)
#
#   # Add grouping
#   if(ggfun == 'geom_line' && !is.null(group) && !'group' %in% names(out$mapping)) {
#     out$mapping <- structure(append(out$mapping, aes_string(group = group)),
#                              class = 'uneval')
#   }
#
#   return(out)
#
# }
