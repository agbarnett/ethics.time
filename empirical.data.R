# empirical.data.R
# empirical data on ethics and SSA times
# Sep 2019

### a) empirical data from REACH
# from U:\Research\Projects\ihbi\aushsi\aushsi_barnetta\REACH\ethics.paper\ethics.R
load('U:\\Research\\Projects\\ihbi\\aushsi\\aushsi_barnetta\\REACH\\ethics.paper\\REACH.times.RData')
barnett = select(times, begin, submit, approval, SSA.start, SSA.end, with) %>%
  mutate(id = 1:n(),
         ethics.time = approval - begin,
         ssa.time = SSA.end - SSA.start) %>%
  select(id, ethics.time, ssa.time) %>%
  gather(ethics.time, ssa.time, key='app', value='time') %>%
  mutate(id = as.numeric(as.factor(id)), # remove missing IDa
         study = 'Barnett',
         which = ifelse(app=='ethics.time', "HREC", 'SSA')) %>%
  select(-app) %>%
  filter(!is.na(time))
# what about withdrawn?

### b) empirical distribution of times from Duplancic et al (2019), extracted using https://apps.automeris.io/wpd/
# low-risk non-interventional study
# DOI: doi.org/10.1111/imj.14158
duplancic = read.csv(header=TRUE, stringsAsFactors = FALSE, text='
bar,time
Bar0, 15.064935064935087
Bar1, 30.64935064935063
Bar2, 78.44155844155844
Bar3, 24.93506493506491
Bar4, 69.61038961038959
Bar5, 40.51948051948052
Bar6, 32.72727272727272
Bar7, 68.05194805194803
Bar8, 62.857142857142826
Bar9, 47.792207792207805
Bar10, 21.81818181818185
Bar11, 218.70129870129867
Bar12, 83.63636363636363
Bar13, 125.1948051948052
Bar14, 224.41558441558436
Bar15, 190.1298701298701
Bar16, 150.1298701298701
Bar17, 164.15584415584414
Bar18, 99.2207792207792') %>%
  mutate(id = 1:n(),
    study = 'Duplancic',
    time = round(time),
         which = ifelse(row_number() <=4 , "HREC", 'SSA')) %>% # first four were ethics; SSA = site-specific approval
  select(-bar)
# check stats are same as paper, only out by a day or two
group_by(duplancic, which) %>%
  summarise(med = median(time), min=min(time), max=max(time))

# concatentate two studies
studies =  bind_rows(barnett, duplancic)
# extract times
ethics.times = filter(studies, which=='HREC')$time
ssa.times = filter(studies, which!='HREC')$time
