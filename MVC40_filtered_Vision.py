import numpy as np
import scipy.io as sio
import scipy.signal as signal
import entropy
import afa
import pandas as pd
import fnmatch
import re
import os
from datetime import datetime
import csv


pd.set_option('expand_frame_repr', False)

rootDir = os.getcwd()

# First run
includes = ['*.mat']
excludes = ['*range*', '*cond2*', '*cond3*', '*cond5*']

# # Later runs
# with open ('Subjects_40MVC.csv') as csvfile:
#     subject_file = csv.reader(csvfile)
#     excludes = []
#     for row in subject_file:
#         sub = row[1]
#         excludes.append(sub)
#
# excludes = set(excludes)
# excludes = list(excludes)
#
# excludes = ["*" + sub + "*" for sub in excludes]
#
# includes = ['*.mat']
# excludes = ['*Prac*', '*Bad*', '*bad*', '*range*', '*cond2*', '*cond3*', '*cond4*']

includes = r'|'.join([fnmatch.translate(x) for x in includes])
excludes = r'|'.join([fnmatch.translate(x) for x in excludes])or r'$.'

extract_subject = re.compile(r'S\d+').findall
extract_trial = re.compile(r'tr\d+').findall
extract_condition = re.compile(r'cond\d+').findall

data_mmse = []
detrended_mmse = []
filtered_data = []
runningtime = []
subjects = []
r = 0.2
scale = 34

b, a = signal.butter(9, 20/(100/2))

startFull = datetime.now()
for root, dirs, files in os.walk(rootDir):

    files = [os.path.join(root, f) for f in files]
    files = [f for f in files if not re.match(excludes, f)]
    files = [f for f in files if re.match(includes, f)]

    for fname in files:
        condition = extract_condition(fname)[0]
        subject = extract_subject(fname)[0]
        trial = extract_trial(fname)[0]

        trial_file = sio.loadmat(fname, squeeze_me=True, struct_as_record=False)
        waveform = trial_file['subwave']
        center = trial_file['cent']

        waveform_trim = np.delete(waveform, range(2900,3000))
        waveform_trim = np.delete(waveform_trim, range(0, 400))
        waveform_filtered = signal.filtfilt(b, a, waveform_trim)

        trial_data = waveform_filtered.tolist()
        trial_data.extend([subject, condition, trial, center])
        filtered_data.append(trial_data)

        # Compute MMSE for the raw signal
        mmse = entropy.modified_multiscale_entropy(waveform_filtered, tau=scale, r=r)
        data_mmse.append(np.append(mmse, [subject, trial]))

        # Compute MMSE for the detrended signal.
        # Apply detrendeding using the method in AFA.
        dt, t = afa.detrending_method(waveform_filtered, seg_len=129, fit_order=2)
        dt_mmse = entropy.modified_multiscale_entropy(dt, tau=scale, r=r)
        detrended_mmse.append(np.append(dt_mmse, [subject, trial]))

        print(fname, 'completed')

        subjects.append(subject)

print("done")
stopFull = datetime.now()

mmse_data = pd.DataFrame(data_mmse)
detrended_data = pd.DataFrame(detrended_mmse)
subject_data = pd.DataFrame(subjects)
rawdata_data = pd.DataFrame(filtered_data)


tmp = [i+1 for i in range(scale)]
tmp.extend(['Subject', 'Trial'])
mmse_data.columns = tmp
detrended_data.columns = tmp

mmse_data_long = pd.melt(mmse_data, id_vars = ['Subject', 'Trial', ],
                         value_vars = [i for i in range(1,scale+1)])

mmse_data_long.rename(columns={'variable': 'Scale', 'value': 'MMSE'}, inplace = True)

detrended_data_long = pd.melt(mmse_data, id_vars=['Subject', 'Trial', ],
                         value_vars=[i for i in range(1, scale + 1)])

detrended_data.rename(columns={'variable': 'Scale', 'value': 'MMSE'}, inplace=True)

# First Run
mmse_data_long.to_csv("MMSE_Vision.csv", header = True)
subject_data.to_csv("Subjects_Vision.csv", header = True)
rawdata_data.to_csv("Raw_Vision.csv", header = True)
detrended_data_long.to_csv("Detrended_Vision.csv", header = True)

# Later Runs
mmse_data_long.to_csv("MMSE_MVC40_r015.csv", mode = 'a', header = False)
subject_data.to_csv("Subjects_40MVC.csv", mode = 'a', header = False)
rawdata_data.to_csv("Raw_MVC40.csv", mode = 'a', header = False)