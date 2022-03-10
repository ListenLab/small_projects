library(audio)
library(ggplot2)
library(dplyr)
library(cowplot)
rm(list = ls())

read_wave <- function(filename, samplerate = 44100){
  audio::load.wave(filename) %>%
    as.numeric() %>%
    data.frame(
      sound = gsub(".wav","", filename),
      time = seq(0, by = 1/samplerate, length.out = length(.)),
      value = .
    ) %>% return()
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# read the continuum 
setwd("L:\\Manuscripts\\Reconsidering_speech_stimuli\\vot_figure\\audio")
df_cont_a <- 
  list.files(pattern = ".wav", full.names = TRUE) %>%
  lapply(., read_wave) %>%
  bind_rows() %>%
  mutate(sound = substr(sound, nchar(sound), nchar(sound))) %>%
  mutate(vowel = "a",
         step = as.numeric(sound)) %>%
  # just keep the middle of the continuum (not endpoints)
  dplyr::filter(step %in% 2:6)

df_cont_i <- 
  list.files(pattern = ".wav", full.names = TRUE) %>%
  lapply(., read_wave) %>%
  bind_rows() %>%
  mutate(sound = substr(sound, nchar(sound), nchar(sound))) %>%
  mutate(vowel = "i",
         step = as.numeric(sound)) %>%
  # just keep the middle of the continuum (not endpoints)
  dplyr::filter(step %in% 2:6)

# indicate where the aspiration is
# VOT is step # * 10 ms, and burst begins at 50 ms
df_cont_a$asp <- with(df_cont_a, 
                      ifelse(time > 0.05 & time < (0.05 + (step*0.01)),"yes","no"))

df_cont_i$asp <- 
  with(df_cont_i, ifelse(time > 0.05 & time < (0.05 + (step*0.01)), "yes","no"))

# plot the continuum
px_cont_a <- df_cont_a %>%
  ggplot()+
  aes(x = time, y = value, color = asp)+
  geom_line(size = 0.5)+
  scale_color_manual(values = c(`yes` = "gray40", `no` = "gray80"))+
  coord_cartesian(ylim=c(-0.5, 0.5),
                  xlim = c(0, 0.56),
                  expand = FALSE)+
  theme_void()+
  theme(strip.text = element_blank())+
  theme(legend.position = "none")+
  facet_grid(. ~ step)

px_cont_i <- px_cont_a %+% df_cont_i

# blank white space to insert in assembled plots
px_blank <- ggplot()+geom_blank()+theme_void()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# spectrograms 
# you can also source this from 
# https://github.com/ListenLab/Praat/blob/master/praat_spectrogram_functions.R
setwd("L:\\Manuscripts\\Reconsidering_speech_stimuli\\vot_figure\\spectrograms")
source("praat_spectrogram_functions.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# put all spectrograms into one data frame
dfsp <- 
  lapply(c(paste0(c("da","ta","di","ti"),".Spectrogram")), 
         function(x){
           convert_spectrogram_to_df(x) %>%
             pre_emphasize() %>% 
             constrain_dynamic_range(column = "Level_preemp",dynamic_range = 120) %>%
             mutate(sound = substr(x,1,2))
         }) %>%
  bind_rows()

# draw spectrograms
px_sp_da <- ggplot(dfsp[dfsp$sound=="da",])+
  aes(x = Time, y=Frequency, fill=Level_preemp_dr)+
  geom_tile()+
  scale_fill_gradient(high="black", low="white", na.value = "white")+
  coord_cartesian(ylim=c(0, 5000),
                  xlim = c(0, 0.56),
                  expand = FALSE)+
  scale_y_continuous(breaks = seq(0,4000,2000),
                     name = "Frequency (Hz)")+
  xlab("Time (s)")+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# px_sp_da

# the other syllables (sub in new data frames in the plot)
px_sp_ta <- px_sp_da %+% dfsp[dfsp$sound=="ta",]
px_sp_di <- px_sp_da %+% dfsp[dfsp$sound=="di",]
px_sp_ti <- px_sp_da %+% dfsp[dfsp$sound=="ti",]

# markers on the F1 onset
f1_size <- 3
px_sp_da_m <- px_sp_da +
  annotate("point", fill = "firebrick", color = "white",stroke = 1, 
           x = 0.12, y = 300, size = f1_size, shape = 21)

px_sp_ta_m <- px_sp_ta +
  annotate("point", fill = "steelblue3", color = "white",stroke = 1, 
           x = 0.12, y = 800, size = f1_size, shape = 24)

px_sp_di_m <- px_sp_di +
  annotate("point", fill = "firebrick", color = "white", stroke = 1, 
           x = 0.12, y = 300, size = f1_size, shape = 21)

px_sp_ti_m <- px_sp_ti +
  annotate("point", fill = "firebrick", color = "white",stroke = 1, 
           x = 0.12, y = 300, size = f1_size, shape = 21)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Arrange everything
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# continuum with extra white space above & below
px_cont_a_border <- 
  cowplot::plot_grid(px_blank, px_cont_a, px_blank, nrow = 3, rel_heights = c(1,2,1))

px_cont_i_border <- 
  cowplot::plot_grid(px_blank, px_cont_i, px_blank, nrow = 3, rel_heights = c(1,2,1))

# top row: wav & spect da, da-ta continuum, wav & spect ta
pz_top <- cowplot::plot_grid(px_sp_da_m, px_cont_a_border, px_sp_ta_m,
                             nrow = 1, rel_widths = c(2, 2.7, 2))
# extra space in middle row
pz_mid <- px_blank

# bottom row: wav & spect di, di-ti continuum, wav & spect ti
pz_bottom <- cowplot::plot_grid(px_sp_di_m, px_cont_i_border, px_sp_ti_m,
                                nrow = 1, rel_widths = c(2, 2.7, 2))

# put it all together
VOT_figure <- 
  cowplot::plot_grid(pz_top, pz_mid, pz_bottom,
                     nrow = 3, rel_heights = c(4,0.2,4))
VOT_figure

# save it
setwd("L:\\Manuscripts\\Reconsidering_speech_stimuli\\Figures")
ggsave(VOT_figure, file = "VOT_figure.png",
       height = 5, width = 8.5, dpi = 600)

# End