#!/bin/bash
model=$(<../R/utils/modelchoice.txt)
echo "Running model inference for model chosen variant:"
echo "************************************************************"
echo "         $model"
echo "************************************************************"
R --slave --vanilla --args < ../R/2stanfitting.R $model
