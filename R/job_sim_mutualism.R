
job_ABC <- function(param_set){

  load("data/mutualism_pars_pool.RData")
  # param_space <- read.csv2(file = 'data/DAISIE_ABC.csv')

  message("Running param set: ", param_set)



  obs_sim_pars <- mutualism_pars_pool[[param_set]]
  sim <- sim_mutualism(simtime = 1,
                       replicates =1,
                       mutualism_pars =  obs_sim_pars)
  return ()
}

###### Scripts/run_ABC_peregrine.R
args <- commandArgs(TRUE)


param_set <- as.numeric(args[2])
idparsopt_lac <- as.numeric(args[3])
idparsopt_mu <- as.numeric(args[4])
idparsopt_gam <- as.numeric(args[5])
idparsopt_laa <- as.numeric(args[6])
idparsopt_lac2 <- as.numeric(args[7])
idparsopt_mu2 <- as.numeric(args[8])
idparsopt_gam2 <- as.numeric(args[9])
idparsopt_laa2 <- as.numeric(args[10])
idparsopt_trans <- as.numeric(args[11])
idparsopt_trans2 <- as.numeric(args[12])
sim_model <- args[13]
idparsopt_all <- c(idparsopt_lac,idparsopt_mu,idparsopt_gam,idparsopt_laa,
                   idparsopt_lac2,idparsopt_mu2,idparsopt_gam2,idparsopt_laa2,
                   idparsopt_trans,idparsopt_trans2)
idparsopt <- which(idparsopt_all == 1)


run_ABC(
  param_space_name = args[1],
  param_set = as.numeric(args[2]),
  idparsopt = as.numeric(idparsopt),
  sim_model = sim_model,
  save_output = save_output
)

####bash/parm_set.sh
#!/bin/bash
#SBATCH --time=7-12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=est_TR
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_space_name=${1}
param_set=${2}
idparsopt_lac=${3}
idparsopt_mu=${4}
idparsopt_gam=${5}
idparsopt_laa=${6}
idparsopt_lac2=${7}
idparsopt_mu2=${8}
idparsopt_gam2=${9}
idparsopt_laa2=${10}
idparsopt_trans=${11}
idparsopt_trans2=${12}
sim_model=${13}


ml R
Rscript TraisieABC/scripts/run_ABC_peregrine.R ${param_space_name} \

#####
#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=1GB
#SBATCH --partition=short


# Start script
ml R
Rscript -e "remotes::install_github('xieshu95/TraisieABC@master')"


for_length=`wc -l TraisieABC/data/${param_space_name}.csv | cut -f1 -d' '`
for_length=$(( ${for_length} - 1 ))

for (( param_set = 1; param_set <= $for_length; param_set++ ))
  do
sbatch TraisieABC/bash/submit_run_ABC_param_set.sh ${param_space_name} \
${param_set} \
${idparsopt_lac} \
${idparsopt_mu} \
${idparsopt_gam} \
${idparsopt_laa} \
${idparsopt_lac2} \
${idparsopt_mu2} \
${idparsopt_gam2} \
${idparsopt_laa2} \
${idparsopt_trans} \
${idparsopt_trans2} \
${sim_model}
