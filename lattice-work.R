with(complete_harp, 
  plot3d( 
  x=TST_diary, 
  y=TST_acti,
  z=ISI, 
  col = "blue",
  type = 'p',
  xlab="sTST", ylab="oTST", zlab="ISI")
)

library(rgl)

plot3d(lm_additive, plane.col = 'blue')

plot3d(lm_piece, plane.col = 'blue')




help(wireframe)

lm_difference <- lm(ISI~dTST, data = complete_harp)

h <- g[,c(1,2,6)]

# Scatterplot with regression line for conventional ratio scores
ggplot(complete_harp, mapping = aes(x = dTST, y = ISI)) +
  geom_point() +
  geom_line(aes(y = lm_difference$fitted.values), color = 'red')

# Multiple segmented with estimated break points
lm_piece2 <- segmented(lm_additive, seg.Z =~TST_diary+TST_acti, psi=list(TST_diary = 350, TST_acti = 350))

# Predicted values from generated x y grid
g$pred.z <- predict.segmented(lm_piece2, g)

# Plot
wireframe(pred.z~TST_diary * TST_acti, g,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          shade= TRUE, drape = FALSE, scales = list(arrows=FALSE))

# rgl plot of unconstrained piecewise regression
plot3d(x = g$TST_diary, y = g$TST_acti, z = g$pred.z, xlim = c(0,400), ylim = c(0,400), zlim = c(0,14))

# rgl plot of unconstrained piecewise regression
plot3d(x = g$TST_diary, y = g$TST_acti, z = g$ISI)

surface3d(x = g$TST_diary, y = g$TST_acti, z = g$pred.z, xlim = c(0,400), ylim = c(0,400), zlim = c(0,14))


wireframe(complete_harp$ISI~complete_harp$TST_diary + complete_harp$TST_acti,
          xlim=c(-400,400), ylim = c(-400,400), zlim = c(0,14),
          shade = TRUE)

complete_harp$predicted <- predict(lm_additive, complete_harp)


range(complete_harp$WsTST)
range(complete_harp$WoTST)

range(complete_harp$TST_diary)
range(complete_harp$TST_acti)

length <- 10
TST_diary <- seq(0, 681, length.out = length)
TST_acti <- seq(113, 622, length.out = length)
W <- c(rep(0, 50), rep(1, 50))
WsTST <- seq(0, 600, length.out = length)
WoTST <- seq(0, 600, length.out = length)

g <- expand.grid(TST_diary = TST_diary, TST_acti = TST_acti)

g <- expand.grid(TST_diary = TST_diary, TST_acti = TST_acti,
                 W = W, WsTST = WsTST, WoTST = WoTST)
g$ISI <- predict(lm_piece, g)


g$pred.isi <- predict(lm_piece, g)

g$ISI <- predict(lm_additive, g)

sem.isi <- predict(sem_piece, g)


wireframe(ISI~TST_diary + TST_acti, g, groups = W,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          shade= TRUE, drape = FALSE, scales = list(arrows=FALSE))

wireframe(ISI~TST_diary + TST_acti + W + WsTST+ WoTST, g,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          shade= TRUE, drape = FALSE, scales = list(arrows=FALSE))

levelplot(ISI~TST_diary + TST_acti + W + WsTST + WoTST, g,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          shade= TRUE, drape = FALSE, scales = list(arrows=FALSE))


wireframe(z~x * y, g.bad,
          xlim=c(-200,200), ylim = c(-200,200), zlim = c(0,14),
          shade= TRUE, drape = FALSE, scales = list(arrows=FALSE))

wireframe(ISI~TST_diary + TST_acti, h,
          xlim=c(0,400), ylim = c(0,400), zlim = c(0,14),
          shade= TRUE, drape = FALSE, scales = list(arrows=FALSE))

cloud(complete_harp$IST~ complete_harp$TST_diary + complete_harp$TST_acti)



rsa1 <- RSA(ISI~TST_diary + TST_acti, data = complete_harp, models = c("absdiff", "absunc"), center = "pooled")
plot(rsa1, model = "absdiff")
plot(rsa1, model = "absunc")

a<-plot(rsa1, model = "absunc")


g.bad <- data.frame("x" = bad.x, "y" = bad.y, "z" = bad.z)



length <- 100
TST_diary <- seq(0, 681, length.out = length)
TST_acti <- seq(113, 622, length.out = length)
W <- c(rep(0, 50), rep(1, 50))
WsTST <- seq(0, 600, length.out = length)
WoTST <- seq(0, 600, length.out = length)

g <- expand.grid(TST_diary = TST_diary, TST_acti = TST_acti, W = W, WsTST = WsTST, WoTST = WoTST)

g <- expand.grid(TST_diary = TST_diary, TST_acti = TST_acti)

g$ISI <- predict(lm_piece, g)

length <- lm_piece$fitted.values %>% length()

sem_piece <- sem(model = 'ISI~ b1*TST_diary + b2*TST_acti + w1*W + w2*WsTST + w3*WoTST', data = complete_harp)



m.absunc <- paste(paste0(DV, " ~ b1*", IV1, " + b2*", 
                         IV2, " + w1*W + w2*W_", IV1, " + w3*W_", IV2), 
                  ifelse(breakline == FALSE, "w1==0", ""), add, 
                  sep = "\n")

df <- complete_harp[,c("TST_diary", "TST_acti", "pred.Z")]

df <- as.matrix(df)

wireframe(df, pred.Z~TST_diary + TST_acti)


wireframe(as.matrix(h))


#=============================================  Code for the graphs  ===========================================

# Store regression coefficients in a vector
C <- lm_piece$coefficients

# Generate vectors of IV values
TST_diary <- seq(0, 700, length.out = length)
TST_acti <- seq(0, 700, length.out = length)

# Generate full grid of IV values
G <- expand.grid(TST_diary = TST_diary, TST_acti = TST_acti)

# Predicted ISI values conditional on W
G$pred.z <- ifelse(G$TST_diary < G$TST_acti,
                   with(G, C[1] + C[2]*TST_diary + C[3]*TST_acti + C[4] + C[5]*TST_diary + C[6]*TST_acti), 
                   with(G, C[1] + C[2]*TST_diary)
                   )

# colpal <- c("#A50026","#D73027","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#D9EF8B","#A6D96A","#66BD63","#1A9850","#006837")
# cex.axesLabel=1
# CK <- list(labels=list(cex=cex.axesLabel))
colpal <- hcl.colors(100, palette = "viridis", rev= TRUE)

# lattice plot of unconstrained piecewise regression
wireframe(pred.z~TST_diary + TST_acti, G,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          xlab = "sTST", ylab = "oTST", zlab = "ISI",
          drape = TRUE, scales = list(arrows=FALSE),
          col.regions = colpal, 
          #colorkey = TRUE, # added by default i think
          screen = list(z = 30, x = -60),
          )

#### ABSOLUTE difference model (constrained)

# Run restriktor absolute difference score model
lm_absolute.r <- restriktor(lm_piece, constraints = 'TST_diary == -TST_acti, W == 0, WsTST == -WoTST, WsTST = -2*TST_diary', se = 'boot.standard')

# Store regression coefficients in a vector
C.a <- coef.restriktor(lm_absolute.r)

# Generate full grid of IV values
G.a <- expand.grid(TST_diary = TST_diary, TST_acti = TST_acti)

# Predicted ISI values conditional on W
G.a$pred.z <- ifelse(G.a$TST_diary < G.a$TST_acti,
                   with(G.a, C.a[1] + C.a[2]*TST_diary + C.a[3]*TST_acti + C.a[4] + C.a[5]*TST_diary + C.a[6]*TST_acti), 
                   with(G.a, C.a[1] + C.a[2]*TST_diary + C.a[3]*TST_acti)
                  )

# lattice plot of absolute difference score model
wireframe(pred.z~TST_diary + TST_acti, G.a,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          xlab = "sTST", ylab = "oTST", zlab = "ISI",
          drape = TRUE, scales = list(arrows=FALSE),
          col.regions = colpal, 
          #colorkey = TRUE, # added by default i think
          screen = list(z = 30, x = -60),
          )




####################################=============

# lattice plot of unconstrained piecewise regression
wireframe(pred.z~TST_diary + TST_acti, dat,
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,14),
          shade= TRUE, drape=TRUE, col.regions = pal)

# rgl plot of unconstrained piecewise regression (edges aren't joing up)
plot3d(x = G$TST_diary, y = G$TST_acti, z = G$pred.z)



p1 <- wireframe(z ~ x*y, new2,  drape=TRUE, 
                scales 	= list(arrows = FALSE, cex=cex.tickLabel, col = axesCol, font = 1, tck=tck, distance=distance),
                col.regions=pal, colorkey=CK, 
                par.settings = list(
                  axis.line = list(col = "transparent"), 
                  layout.heights = list(top.padding=pad, bottom.padding=pad), 
                  layout.widths=list(left.padding=pad, right.padding=pad),
                  box.3d = list(col=boxCol)), 
                axes	= axes,
                axesList= axesList, 
                SPs		= SP.text,
                COEFS	= COEFS, 
                panel.3d.wireframe = mypanel2,
                x.points=xpoints, y.points=ypoints, z.points=zpoints)


### FNVFKNAFNJAFNAF

# Predicted ISI values from absolute difference score model


G$pred.zabs <- predict(lm_absolute, G[,1:2])


``` {r diffplots}
# 
# complete_harp <- na.omit(harp)
# 
# lm_diffgraph <- lm(ISI ~ dTST, data = complete_harp)
# 
# ggplot(complete_harp, mapping = aes(x = dTST, y = ISI)) +
#   geom_point() +
#   geom_line(aes(y = lm_diffgraph$fitted.values), color = 'red')
# 
# ggplot(complete_harp, mapping = aes(x = dTST, y = ISI)) +
#   geom_point() +
#   geom_line(aes(y = lm_diffgraph$fitted.values), color = 'red')
# 

```
library(RSA)


r1 <- RSA(ISI~TST_diary + TST_acti, data = harp, models = c("all"), cubic = TRUE, center = "pooled", scale = "pooled")

compare2(r1, m1 = "additive", m2 = "absunc")
compare2(r1, m1 = "full", m2 = "RRCA")

plot(r1, model = "RRCA")
plot(r1, model = "cubic")

compare(r1)
summary(r1)    

# Nice RSA on the HARP data although the asymmetric is opposite to discrepancy hypothesis
plot(r1, model = "RRCA", xlim = c(-2, 2), ylim = c(-2, 2))

#################+=======================================================================

wireframe(z ~ x * y, new2, drape = TRUE, scales = list(arrows = FALSE, 
                                                       cex = cex.tickLabel, col = axesCol, font = 1, tck = tck, 
                                                       distance = distance), xlab = list(cex = cex.axesLabel, 
          label = xlab, rot = label.rotation[["x"]]), ylab = list(cex = cex.axesLabel, 
          label = ylab, rot = label.rotation[["y"]]), zlab = list(cex = cex.axesLabel, 
          label = zlab, rot = label.rotation[["z"]]), zlim = zlim, 
          main = list(cex = cex.main, label = main), screen = rotation, 
          at = at, col.regions = pal, colorkey = CK, par.settings = list(axis.line = list(col = "transparent"), 
          layout.heights = list(top.padding = pad, bottom.padding = pad), 
          layout.widths = list(left.padding = pad, right.padding = pad), 
          box.3d = list(col = boxCol)), axes = axes, axesList = axesList, 
          SPs = SP.text, COEFS = COEFS, panel.3d.wireframe = mypanel2, 
          x.points = xpoints, y.points = ypoints, z.points = zpoints)

col.pal <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", 
             "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", 
             "#66BD63", "#1A9850", "#006837")

at <- seq(0, 14, length.out = length(col.pal) - 1)

cex.axesLabel <- 1

col.key <- list(labels = list(cex = cex.axesLabel))

mypanel2 <- function(x, y, z, xlim, ylim, zlim, xlim.scaled, ylim.scaled, zlim.scaled, axes, axesList, x.points = NULL, 
y.points = NULL, z.points = NULL, SPs = "")

# lattice plot of unconstrained piecewise regression
wireframe(pred.z~TST_diary + TST_acti, G, 
          at = seq(0, 14, length.out = length(col.pal) - 1),
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,20),
          xlab = "sTST", ylab = "oTST", zlab = "ISI",
          drape = TRUE, scales = list(arrows=FALSE),
          col.regions = col.pal, 
          colorkey = col.key,
          par.settings = list(axis.line = list(col = "transparent")),
          screen = list(z = 30, x = -60),
          box.3d = list(col = "transparent"),
  #        panel.3d.wireframe = (wireframe = FALSE),
# shade = TRUE,
# shade.colors.palette = makeShadePalette(col.pal, pref = 0.1))
)

# lattice plot of absolute difference score model
wireframe(pred.z~TST_diary + TST_acti, G.a, 
          at = seq(0, 14, length.out = length(col.pal) - 1),
          xlim=c(0,600), ylim = c(0,600), zlim = c(0,20),
          xlab = "sTST", ylab = "oTST", zlab = "ISI",
          drape = TRUE, scales = list(arrows=FALSE),
          col.regions = col.pal, 
          colorkey = col.key,
          par.settings = list(axis.line = list(col = "transparent")),
          screen = list(z = 30, x = -60),
          box.3d = list(col = "transparent"),
                  panel.3d.wireframe = "panel.3d.contour",
#          shade = TRUE,
#          shade.colors.palette = makeShadePalette(col.pal, pref = 0.1))
)


panel.3d.contour <-
  function(x, y, z, rot.mat, distance,
           nlevels = 20, zlim.scaled, ...)
  {
    add.line <- trellis.par.get("add.line")
    panel.3dwire(x, y, z, rot.mat, distance,
                 zlim.scaled = zlim.scaled, ...)
    clines <-
      contourLines(x, y, matrix(z, nrow = length(x), byrow = TRUE),
                   nlevels = nlevels)
    for (ll in clines) {
      m <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[2]),
                            rot.mat, distance)
      panel.lines(m[1,], m[2,], col = add.line$col,
                  lty = add.line$lty, lwd = add.line$lwd)
    }
  }



hcl.colors(10, "Inferno"), pref = 0.2

mypanel2 <- function(x, y, z, xlim, ylim, zlim, xlim.scaled, 
                     ylim.scaled, zlim.scaled, axes, axesList, x.points = NULL, 
                     y.points = NULL, z.points = NULL, SPs = "", ...) {
                     RESCALE.Z <- function(z1) {
                     Z2 <- zlim.scaled[1] + diff(zlim.scaled) * (z1 - 
                     zlim[1])/diff(zlim)
                     return(Z2)
                     }
                     RESCALE <- function(n) {
                       X2 <- xlim.scaled[1] + diff(xlim.scaled) * (n$X - 
                                                                     xlim[1])/diff(xlim)
                       Y2 <- ylim.scaled[1] + diff(ylim.scaled) * (n$Y - 
                                                                     ylim[1])/diff(ylim)
                       Z2 <- zlim.scaled[1] + diff(zlim.scaled) * (n$Z - 
                                                                     zlim[1])/diff(zlim)
                       df <- data.frame(X = X2, Y = Y2, Z = Z2)
                       df <- df[df$X >= min(xlim.scaled) & df$X <= 
                                  max(xlim.scaled) & df$Y >= min(ylim.scaled) & 
                                  df$Y <= max(ylim.scaled) & df$Z >= min(zlim.scaled) & 
                                  df$Z <= max(zlim.scaled), ]
                       return(df)
                     }
                     
RSA_absunc <- RSA(ISI~TST_diary + TST_acti, data = complete_harp, models = c("absdiff", "absunc"), center = "pooled", breakline = TRUE)

RSA_absunc <- RSA(ISI~TST_diary + TST_acti, data = complete_harp, models ="absunc", estimator = "ML", se = "standard")

plot(RSA_absunc, model = "absunc", points = FALSE, bag = FALSE)
plot(RSA_absunc, model = "absdiff", points = FALSE, bag = FALSE)
