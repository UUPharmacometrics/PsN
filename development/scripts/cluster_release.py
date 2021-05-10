#!/usr/bin/python3

import os
import sys


version = sys.argv[1]


class cd:
    """Context manager for changing the current working directory"""
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)


# with cd('../../pharmpy'):
#    os.system('git pull')

# with cd('../../PsNR'):
#    os.system('git pull')


user = 'rikno764';
host = 'fbv-n67.farmbio.uu.se'
user_host = f'{user}@{host}'

with cd('../..'):
    os.system('make')
    os.system('make release')
    os.system(f"ssh {user_host} 'rm PsN-Source -rf'")
    os.system(f"scp -r PsN-Source {user_host}:.")
    with open('/tmp/install_psn.py', 'wt') as fp:
        fp.write(f"""
import os

os.environ['PATH'] = '/opt/local64/python3/3.8.3/bin:/opt/local64/texlive/2017/bin/x86_64-linux:/opt/local64/slurm7-17.11.4/bin:/home/USER/rikno764/perl5/bin:/usr/lib64/mpich/bin:/opt/local64/R/centos7-current/bin/:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/opt/local64/bin/:/opt/local64/matlab/R2012b/bin:/opt/local64/PsN/bin:/opt/local64/python3/3.8.3/bin:/opt/thinlinc/bin:/home/USER/rikno764/bin'
os.chdir('PsN-Source')
os.system('perl setup.pl')
#os.system('./postinstall {version} -dev')
os.chdir('/opt/local64/PsN/PsN_test_{version.replace(".", "_")}')
os.system('prove unit -r')
os.system('prove system -j6')
""")
    os.system(f"scp /tmp/install_psn.py {user_host}:.")
    os.system(f"ssh {user_host} 'python3 install_psn.py'")
