#####################
### Preliminaries ###
#####################
# Loading source code
library(here)
source(here("Code", "CaseStudy2", "0_Source.R"))

# Read population data
load("Data/Processed/Population/pop_dat.RData")

# Read population data
load("Data/Processed/Shapefiles/shapefiles.RData")

# Subsetting Greater Manchester shapefiles
mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                    'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))
mcr_msoa_region <- subset(ew_msoa_region, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                                  'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

# Loading exposures
load('Output/CaseStudy2/Analysis/DailyAverage_July_2021.RData')

# Relabelling exposures 
out_july2021$nssec5[which(out_july2021$nssec5 == -1)] <- 0

###########################################
### Figure 1 - Scatter plots with lines ###
###########################################
# Getting quarterly averages
QuarterAverage_july2021 <- out_july2021 %>%
  ddply(.(pop_id, sex_label, agegr4_label),
        summarize,
        mean = mean(exposure_gm),
        lower = quantile(exposure_gm, probs = 0.025),
        upper = quantile(exposure_gm, probs = 0.975),
        outdoor = mean(pm25_gm_near))

# Converting labels to character
QuarterAverage_july2021$sex_label <- as.character(QuarterAverage_july2021$sex_label)
QuarterAverage_july2021$agegr4_label <- as.character(QuarterAverage_july2021$agegr4_label)

# Empty datasets for storing coefficients
coeffs_july2021 <- expand.grid(sex_label = sort(unique(QuarterAverage_q12021$sex_label)),
                               agegr4_label = sort(unique(QuarterAverage_q12021$agegr4_label)),
                               intercept = NA,
                               slope = NA)

# Converting labels to character
coeffs_july2021$sex_label <- as.character(coeffs_july2021$sex_label)
coeffs_july2021$agegr4_label <- as.character(coeffs_july2021$agegr4_label)

# Enpty dataset for predictions
preds_july2021 <- NULL

# Loop for each age/sex profile
for (i in 1:nrow(coeffs_july2021)){
  # Only keeping specific age and sex
  lm_dat <- subset(QuarterAverage_july2021,
                   sex_label == coeffs_july2021$sex_label[i] &
                     agegr4_label == coeffs_july2021$agegr4_label[i])
  # Runnign linear model
  mod <- lm(mean ~ outdoor,
            data = lm_dat)
  # Storing coefficients
  coeffs_july2021$intercept[i] <- mod$coefficients[1]
  coeffs_july2021$slope[i] <- mod$coefficients[2]
  # Dataset for predictions
  preds_july2021_tmp1 <- data.frame(sex_label = coeffs_july2021$sex_label[i],
                                    agegr4_label = coeffs_july2021$agegr4_label[i],
                                    outdoor = seq(4, 16, length.out = 100))
  # Predicting from the model
  preds_july2021_tmp2 <- predict(mod,
                                 newdata = preds_july2021_tmp1,
                                 se.fit = TRUE)
  # Getting predictions
  preds_july2021_tmp1$pred <- preds_july2021_tmp2$fit
  preds_july2021_tmp1$lower <- preds_july2021_tmp2$fit - 1.96 * preds_july2021_tmp2$se.fit
  preds_july2021_tmp1$upper <- preds_july2021_tmp2$fit + 1.96 * preds_july2021_tmp2$se.fit
  # Adding outputs
  preds_july2021 <- rbind(preds_july2021, preds_july2021_tmp1)
  # Removing unecessary datasets
  rm(preds_july2021_tmp1, preds_july2021_tmp2, mod)
  # Printing index
  print(i)
}

# Plot with linear models
pdf('Fig1a.pdf', height = 7, width = 14)
ggplot(QuarterAverage_july2021,
       aes(x = outdoor,
           y = mean)) +
  geom_point(size = 0.25) +
  geom_ribbon(data = preds_july2021,
              aes(x = outdoor,
                  ymin = lower,
                  ymax = upper),
              size = 1,
              fill = 'skyblue',
              colour = NA,
              alpha = 0.5,
              inherit.aes = FALSE) +
  geom_line(data = preds_july2021,
            aes(x = outdoor,
                y = pred),
            size = 1,
            colour = 'skyblue') +
  theme_bw() +
  scale_x_continuous(limits = c(0,NA),
                     breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(limits = c(0,NA),
                     breaks = scales::pretty_breaks(8)) +
  geom_abline(intercept = 0, slope = 1, colour = 'red', size = 1)+
  facet_grid(sex_label ~ agegr4_label) +
  labs(x = expression('Average ambient PM'[2.5] * ' (' * mu *'g/m'^3 * ')'),
       y = expression('Average personal exposure to PM'[2.5] * ' (' * mu *'g/m'^3 * ')'))
dev.off()

# Saving coefficients
write_csv(coeffs_july2021, file = 'Coeffs_july2021.csv')

########################################
### Figure 2 - Time series line plot ###
########################################
# Case study 1
QuarterAverage_july2021 <-
  rbind(out_july2021 %>%
          ddply(.(date),
                summarize,
                type = 'Personal exposure',
                mean = mean(exposure_gm - pm25_gm_near),
                lower = quantile(exposure_gm - pm25_gm_near, probs = 0.025),
                upper = quantile(exposure_gm - pm25_gm_near, probs = 0.975)))

# Plotting case study 1
pdf('Fig2a_diff.pdf', width = 10)
ggplot(QuarterAverage_july2021,
       aes(x = date)) +
  geom_line(aes(y = mean),
            size = 1) +
  theme_bw()+
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(-4, 4),
                     breaks = scales::pretty_breaks(8)) +
  geom_hline(yintercept = 0, colour = 'red', size = 1, linetype = 'dotted') +
  labs(x = "Date",
       y = expression('Concentration of PM'[2.5] * ' (' * mu *'g/m'^3 * ')'),
       colour = '')
dev.off()

#############################################
### Figure 3 - Maps of personal exposures ###
#############################################
# Getting quarterly averages
QuarterAverage_july2021 <- out_july2021 %>%
  ddply(.(area_id),
        summarize,
        mean = mean(exposure_gm),
        outdoor = mean(pm25_gm_near),
        diff = mean(exposure_gm - pm25_gm_near))

# Wide to long dataset
QuarterAverage_july2021 <- melt(QuarterAverage_july2021, id.vars=c("area_id"))

# Merging on to shapefiles
QuarterAverage_july2021 <- mcr_msoa %>%
  st_as_sf() %>%
  left_join(QuarterAverage_july2021) %>%
  filter(!is.na(mean))

# Converting to SF object
mcr_msoa_region <- st_as_sf(mcr_msoa_region)

# Colour palettes
colourPalette1 <- rev(colorRampPalette(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))(100))
colourPalette2 <- colorRampPalette(c('blue', 'white', 'red'))(100)

# Plot of personal exposures
p1 <- ggplot(subset(QuarterAverage_july2021, variable == 'mean'),
             aes(fill = value)) +
  geom_sf(colour = NA) +
  geom_sf(data = mcr_msoa_region,
          inherit.aes = FALSE,
          colour = 'black',
          fill = NA,
          size = 0.4) +
  theme_bw()+
  theme(legend.position = 'bottom',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = '',
       y = '',
       fill = '',
       title = '(a)') +
  # Colour scheme
  scale_fill_gradientn(limits = c(3.5,7.5),
                       breaks = seq(3.5, 7.5, length.out = 5),
                       colours = colourPalette1,
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE,
                                              draw.llim = TRUE,
                                              # here comes the code change:
                                              frame.colour = "black",
                                              ticks = TRUE,
                                              nbin = 10,
                                              label.position = "bottom",
                                              barwidth = 13,
                                              barheight = 0.75,
                                              direction = 'horizontal'))

# Plot of ambient concentrations
p2 <- ggplot(subset(QuarterAverage_july2021, variable == 'outdoor'),
             aes(fill = value)) +
  geom_sf(colour = NA) +
  geom_sf(data = mcr_msoa_region,
          inherit.aes = FALSE,
          colour = 'black',
          fill = NA,
          size = 0.4) +
  theme_bw()+
  theme(legend.position = 'bottom',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = '',
       y = '',
       fill = '',
       title = '(b)') +
  # Colour scheme
  scale_fill_gradientn(limits = c(2,9),
                       breaks = seq(2, 9, length.out = 8),
                       colours = colourPalette1,
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE,
                                              draw.llim = TRUE,
                                              # here comes the code change:
                                              frame.colour = "black",
                                              ticks = TRUE,
                                              nbin = 10,
                                              label.position = "bottom",
                                              barwidth = 13,
                                              barheight = 0.75,
                                              direction = 'horizontal'))

# Plot of differences
p3 <- ggplot(subset(QuarterAverage_july2021, variable == 'diff'),
             aes(fill = value)) +
  geom_sf(colour = NA) +
  geom_sf(data = mcr_msoa_region,
          inherit.aes = FALSE,
          colour = 'black',
          fill = NA,
          size = 0.4) +
  theme_bw()+
  theme(legend.position = 'bottom',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = '',
       y = '',
       fill = '',
       title = '(c)') +
  # Colour scheme
  scale_fill_gradientn(limits = c(-3,3),
                       breaks = seq(-3, 3, length.out = 7),
                       colours = colourPalette2,
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE,
                                              draw.llim = TRUE,
                                              # here comes the code change:
                                              frame.colour = "black",
                                              ticks = TRUE,
                                              nbin = 10,
                                              label.position = "bottom",
                                              barwidth = 13,
                                              barheight = 0.75,
                                              direction = 'horizontal'))

# Outputting plot
pdf('Fig3a.pdf', width = 10, height = 4)
multiplot(p1, p2, p3, cols = 3)
dev.off()

###############################################
### Figure 4  - Density plots of exposures ####
###############################################
pdf('Fig4a_nssec5.pdf', width = 10)
ggplot(out_july2021,
       aes(x = exposure_gm)) +
  geom_density(alpha = 0.5,
               fill = 'skyblue') +
  facet_wrap(. ~ nssec5) +
  labs(x = expression('Daily average personal exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * ')'),
       y = "Density",
       fill = "") +
  theme_bw() +
  theme(legend.position = 'bottom')
dev.off()

pdf('Fig4a_AgeGr_Sex.pdf', width = 10)
ggplot(out_july2021,
       aes(x = exposure_gm)) +
  geom_density(alpha = 0.5,
               fill = 'skyblue') +
  facet_grid(sex_label ~ agegr4_label) +
  labs(x = expression('Daily average personal exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * ')'),
       y = "Density",
       fill = "") +
  theme_bw() +
  theme(legend.position = 'bottom')
dev.off()
