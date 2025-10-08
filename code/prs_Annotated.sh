#!/bin/bash

# Specify a job name
#$ -N prs_copd900kwang_Exposome.sh

# --- Parameters for the Queue Master ---
# Project name and target queue
#$ -P hunter.prjc
#$ -q short.qc
#$ -pe shmem 2

# Specify the working directory
#$ -wd /well/hunter/users/byo769/ukb/prs/projects/exposome/copd900k_wang

# Log file locations
#$ -o /well/hunter/users/byo769/ukb/prs/projects/exposome/copd900k_wang/logs/
#$ -e /well/hunter/users/byo769/ukb/prs/projects/exposome/copd900k_wang/logs/

# Print some useful data about the job to help with debugging
echo "------------------------------------------------"
echo "Job ID: $JOB_ID"
echo "SGE Job ID: $SGE_JOB_ID"
echo "Run on host: "`hostname`
echo "Operating system: "`uname -s`
echo "Username: "`whoami`
echo "Started at: "`date`
echo "------------------------------------------------"

# Paths to important files
UKBDATAPATH=/well/ukbb-wtchg/v3/imputation
SAMPLE=/well/hunter/users/byo769/ukb/prs/projects/exposome/ukb22828_c1_b0_v3_s487202.sample
BETAS=inputs/Betas.csv

# Paths to software tools
BGENIXPATH=/apps/well/bgenix/1.1.1/bin/bgenix
CATBGENPATH=/apps/well/bgenix/1.1.1/bin/cat-bgen

# Paths to QC files
IMPUTETSV=/well/hunter/shared/ukb_mfi/ukb_mfi_v3.tsv

# Load software modules
module load PLINK/2.00a2.3_x86_64
module load SQLite/3.29.0-GCCcore-8.3.0

# Name of the resulting PRS
PRS=prs_copd900kwang_20221025

###################################

awk -F, '{ if (NR>1) { print $1 }}' inputs/Betas.csv > outputs/rsidlist.txt

# Run a few chr in parallel
for j in {0..5}
do
  START=$((4*j + 1))
  END=$(( 4*(j+1) < 22 ? 4*(j+1) : 22))
  for ((i=START;i<=END;i++))
  do
    ${BGENIXPATH} -g ${UKBDATAPATH}/ukb_imp_chr${i}_v3.bgen \
-incl-rsids outputs/rsidlist.txt > outputs/chr_${i}.bgen &
  done
  wait
done
  
# Combine the .bgen files for each chromosome into one
cmd=""
for i in {1..22}
do
  cmd=$cmd"outputs/chr_${i}.bgen "
done
${CATBGENPATH} -g  $cmd -og outputs/initial_chr.bgen -clobber

 Remove the individual chromosome files
for i in {1..22}
do
  rm outputs/chr_${i}.bgen
done

${BGENIXPATH} -g outputs/initial_chr.bgen -index -clobber
${BGENIXPATH} -g outputs/initial_chr.bgen -list > outputs/initial_chr.txt


########################################################

# Import the betas into the sqlite database index file as a table called Betas
sqlite3 outputs/initial_chr.bgen.bgi "DROP TABLE IF EXISTS Betas;"
sqlite3 -separator "," outputs/initial_chr.bgen.bgi ".import ${BETAS} Betas"

sqlite3 outputs/initial_chr.bgen.bgi "DROP TABLE IF EXISTS Joined;"
# And inner join it to the index table (Variants), making a new table (Joined)
# By joining on alleles as well as chromosome and position 
# we can ensure only the relevant alleles from any multi-allelic SNPs are retained
sqlite3 -header -csv outputs/initial_chr.bgen.bgi "CREATE TABLE Joined AS SELECT Variant.*, Betas.chr_name, -Betas.effect_weight AS Beta FROM Variant INNER JOIN Betas ON Variant.chromosome = printf('%02d', Betas.chr_name) AND Variant.position = Betas.chr_position AND Variant.allele1 = Betas.effect_allele AND Variant.allele2 = Betas.other_allele UNION SELECT Variant.*, Betas.chr_name, Betas.effect_weight AS Beta FROM Variant INNER JOIN Betas ON Variant.chromosome = printf('%02d', Betas.chr_name) AND Variant.position = Betas.chr_position AND Variant.allele1 = Betas.other_allele AND Variant.allele2 = Betas.effect_allele;"

# Filter the .bgen file to include only the alleles specified in the Betas for each SNP 
${BGENIXPATH} -g outputs/initial_chr.bgen -table Joined  > outputs/single_allelic.bgen
${BGENIXPATH} -g outputs/single_allelic.bgen -index -clobber
${BGENIXPATH} -g outputs/single_allelic.bgen -list > outputs/single_allelic.txt


########################################################
# Convert to plink and output allele frequencies

plink2 --bgen outputs/single_allelic.bgen ref-first \
--hard-call-threshold 0.1 \
--sample ${SAMPLE} \
--memory 15000 \
--set-all-var-ids @:#_\$r_\$a \
--freq \
--make-pgen \
--out outputs/raw 


########################################################
# Identify ambiguous SNPs

awk '/^[^#]/ { if( $5>0.49 && $5<0.51 && ( ($3=="A" && $4=="T") || ($4=="T" && $3=="A") || ($3=="C" && $4=="G") || ($4=="G" && $3=="C") ) ) { print $0 }}' outputs/raw.afreq > outputs/exclrsIDs_ambiguous.txt

# Exclude ambiguous SNPs and SNPs with imputation info < 0.4

plink2 --pfile outputs/raw \
--memory 15000 \
--exclude outputs/exclrsIDs_ambiguous.txt \
--extract-col-cond ${IMPUTETSV} 9 10 --extract-col-cond-min 0.4 \
--maf 0.005 \
--rm-dup retain-mismatch \
--write-snplist \
--make-pgen \
--out outputs/snpQC 

########################################################
# Create score file with same SNP IDs (chr:pos_ref_alt) and weights

sqlite3 "" <<EndOfSqlite3Commands
.mode list
.separator ' '
ATTACH 'outputs/initial_chr.bgen.bgi' AS db;
.output inputs/score.txt
SELECT chr_name || ':' || position || '_' ||allele1 || '_' || allele2, allele2, Beta FROM Joined;
EndOfSqlite3Commands

# Calculate PRS

plink2 --pfile outputs/raw \
--memory 15000 \
--extract outputs/snpQC.snplist \
--score inputs/score.txt no-mean-imputation \
--out outputs/${PRS}


