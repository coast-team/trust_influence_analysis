# Analysis using Jupyter


#### Contents

- `./analysis/all_data/`
  Directory in which, you **have to make a copy** of the collected data (`/datafiles_ztree/all_data/`) into this directory that will be mounted in the Jupyter docker container
- `./analysis/figures/`
  Output directory of generated figures
- `./analysis/analysis-excerpt.ipynb`
  Exceprt of Vinh's analysis (only computation of average sending proportions in different games)
- `./analysis/process_data-excerpt.R`
  Exceprt of Vinh's R code that performs loading of Ztree data and some computations.

#### Performing analysis in Jupyter

1. Start a jupyter container using the following command:
```bash
docker run --name jupyter --rm -p 8888:8888 -v "$PWD":/home/jovyan/work -e JUPYTER_ENABLE_LAB=yes jupyter/r-notebook start-notebook.sh --NotebookApp.password='sha1:dbd69b005fa5:36b2e12cb1b61224c83bca915b0e48b4a25676cf'
````

2. Launch your favorite web browser and navigate to http://localhost:8888/

3. Connect and load the provided ipython notebook (`worl/analysis/analysis-excerpt.ipynb`).


## Additional Docker Instruction

#### Retrieving Jupyter docker container

```bash
docker pull jupyter/r-notebook
```

See online [documentation](https://jupyter-docker-stacks.readthedocs.io/en/latest/index.html).

#### Starting a container

##### Starting a basic container 

To run a container which remains intact for restart after the notebook server exits, you can use the following command:

```bash
docker run -p 8888:8888 -e JUPYTER_ENABLE_LAB=yes jupyter/r-notebook 
```

The container can be stopped and restarted latter (see `docker ps -a` to retrieve the container id).


##### Starting an *ephemeral* instance with mounted volume

To run an *ephemeral* instance that mounts the current path to `/home/jovyan/work`, you can use the following command:

```bash
docker run --rm -p 8888:8888 -v "$PWD":/home/jovyan/work -e JUPYTER_ENABLE_LAB=yes jupyter/r-notebook 
```

##### Configuring password authentification

First start an ephemeral container
```bash
docker run --rm -p 8888:8888 -e JUPYTER_ENABLE_LAB=yes jupyter/r-notebook
```

Using a web brower, navigate to the running http://localhost:8888/?token=XXX (the token could be retrieved from the console you started the docker container), start a new Python3 console and enter the following code:

```python
from IPython.lib import passwd
passwd()
Enter password:
Verify password:
Out[2]: 'sha1:dbd69b005fa5:36b2e12cb1b61224c83bca915b0e48b4a25676cf'
```

Then you can start a container that allows to authenticate yourself using the chosen password.

```bash
docker run --name jupyter --rm -p 8888:8888 -v "$PWD":/home/jovyan/work -e JUPYTER_ENABLE_LAB=yes jupyter/r-notebook start-notebook.sh --NotebookApp.password='sha1:dbd69b005fa5:36b2e12cb1b61224c83bca915b0e48b4a25676cf'
```



