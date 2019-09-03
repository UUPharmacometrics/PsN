import subprocess
import datetime
import dateutil.parser

# Uses zsh specific globbing

def get_line_count(path):
    wc_output = subprocess.check_output("wc " + path + " -l", shell=True, executable='/usr/bin/zsh')
    final_line = wc_output.decode().splitlines()[-1]
    count = int(final_line.split()[0])
    return count



output = subprocess.check_output(['git', '--no-pager', 'log', "--format=%cI %H"])
output = output.decode()

for line in output.splitlines():
    date_str, commit = line.split() 
    date = dateutil.parser.parse(date_str).replace(tzinfo=None)
    if date >= datetime.datetime(2019, 1, 1):
        subprocess.run(['git', 'checkout', commit], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        bin_count = get_line_count('bin/*')
        lib_count = get_line_count('lib/**/*(.)')
        perl_count = bin_count + lib_count
        r_count = get_line_count('R-scripts/**/*(.)')
        print(date, perl_count, r_count)
        subprocess.run(['git', 'checkout', 'master'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
