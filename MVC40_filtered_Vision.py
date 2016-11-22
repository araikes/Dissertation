import fnmatch
import os
import re
from datetime import datetime

import afa
import entropy
import pandas as pd
import scipy.io as sio
import scipy.signal as signal
import dfa
import spectral

pd.set_option('expand_frame_repr', False)

rootDir = os.getcwd()

# Set includes and excludes for files
includes = ['*.mat']
excludes = ['*range*', '*cond2*', '*cond3*', '*cond5*']

includes = r'|'.join([fnmatch.translate(x) for x in includes])
excludes = r'|'.join([fnmatch.translate(x) for x in excludes])or r'$.'

extract_subject = re.compile(r'S\d+').findall
extract_trial = re.compile(r'tr\d+').findall
extract_condition = re.compile(r'cond\d+').findall

# Set empty dataframes and conditions for computing MMSE, spectral properties, and DFA
data_mmse = []
avg_power = []
dfa_alpha = []
detrended_mmse = []
filtered_data = []
runningtime = []
subjects = []

# Set conditions for computing MMSE, average power, and DFA
r = 0.2
scale = 34
bin_ends = [4.0, 8.0, 12.0]
dfa_lengths = [10, 122]

b, a = signal.butter(9, 20/(100/2))

startFull = datetime.now()
for root, dirs, files in os.walk(rootDir):

    files = [os.path.join(root, f) for f in files]
    files = [f for f in files if not re.match(excludes, f)]
    files = [f for f in files if re.match(includes, f)]

    for fname in files:
        subject = extract_subject(fname)[0]
        trial = extract_trial(fname)[0]

        trial_file = sio.loadmat(fname, squeeze_me=True, struct_as_record=False)
        waveform = trial_file['subwave']
        center = trial_file['cent']

        waveform_trim = spectral.np.delete(waveform, range(2900, 3000))
        waveform_trim = spectral.np.delete(waveform_trim, range(0, 400))
        waveform_filtered = signal.filtfilt(b, a, waveform_trim)

        trial_data = waveform_filtered.tolist()
        trial_data.extend([subject, trial, center])
        filtered_data.append(trial_data)

        # Compute MMSE for the raw signal
        mmse = entropy.modified_multiscale_entropy(waveform_filtered, tau=scale, r=r)
        data_mmse.append(spectral.np.append(mmse, [subject, trial]))

        # Compute average power for the raw signal
        avp = spectral.average_power(ts = waveform_filtered, Fs = 100, bin_ends = bin_ends, norm = True)
        avp.extend([subject, trial])
        avg_power.append(avp)

        # Compute DFA alpha for the raw signal
        alpha = dfa.dfa(time_series = waveform_filtered, bin_range = dfa_lengths, plot_dfa = False)
        dfa_alpha.append([subject, trial, alpha])

        # Compute MMSE for the detrended signal.
        # Apply detrendeding using the method in AFA.
        dt, t = afa.detrending_method(waveform_filtered, seg_len=129, fit_order=2)
        dt_mmse = entropy.modified_multiscale_entropy(dt, tau=scale, r=r)
        detrended_mmse.append(spectral.np.append(dt_mmse, [subject, trial]))

        print(fname, 'completed')

        subjects.append(subject)

print("done")
stopFull = datetime.now()

mmse_data = pd.DataFrame(data_mmse)
detrended_data = pd.DataFrame(detrended_mmse)
avp_data = pd.DataFrame(avg_power)
dfa_data = pd.DataFrame(dfa_alpha)
subject_data = pd.DataFrame(subjects)
rawdata_data = pd.DataFrame(filtered_data)

# MMSE column names
tmp = [i+1 for i in range(scale)]
tmp.extend(['subject', 'trial'])
mmse_data.columns = tmp
detrended_data.columns = tmp

# Average power column names
tmp = ['0-4 Hz', '4-8 Hz', '8-12 Hz', 'subject', 'trial']
avp_data.columns = tmp

# DFA column names
tmp = ['subject', 'trial', 'alpha']
dfa_data.columns = tmp

# Subjects column names
subject_data.columns = ['subject']

# Raw data column names
tmp = [i + 1 for i in range(2500)]
tmp.extend(['subject', 'trial', 'center'])
rawdata_data.columns = tmp

# First Run
mmse_data_long.to_csv("Original Data Files\MMSE.csv", header = True)
subject_data.to_csv("Original Data Files\Subjects.csv", header = True)
rawdata_data.to_csv("Original Data Files\Raw Data.csv", header = True)
detrended_data_long.to_csv("Data Files\Detrended MMSE.csv", header = True)
dfa_data.to_csv("Original Data Files\DFA.csv", header = True)
avp_data.to_csv("Original Data Files\AvP.csv", header = True)