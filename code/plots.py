# -*- coding: utf-8 -*-
# @Author: lily
# @Date:   2020-04-04 17:09:18
# @Last Modified by:   lily
# @Last Modified time: 2020-05-12 05:33:13
import io, os, sys, types, pickle, warnings
from datetime import datetime, timedelta

import pandas as pd
import numpy as np
import scipy.optimize as opt
from scipy import stats
from sklearn import preprocessing
from sklearn.linear_model import LinearRegression

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib import colors as mcolors
import matplotlib.pylab as pl

import seaborn as sns

warnings.filterwarnings('ignore')

master_path = os.getcwd()
if master_path not in sys.path:
	sys.path.append(master_path)
import functions as my_func

""" Params """
cat_color = {'Confirmed':'tab:blue', 
			  'Deaths':'tab:red', 
			  'Recovered':'tab:green', 
			  'Active':'tab:orange', 
			  'Positive':'tab:purple', 
			  'Negative':'tab:olive', 
			  'Democratic':'tab:blue', 
			  'Republican':'tab:red',
			  'Independent':'tab:green'}
cdra_cols = ['Confirmed', 'Deaths', 'Recovered', 'Active']
pn_cols = ['Positive', 'Negative']
party_cols = ['Democratic', 'Republican', 'Independent']

SMALL_SIZE = 10
MEDIUM_SIZE = 12
BIGGER_SIZE = 16
MILLION = 1000000

"""general functions"""
### label bar plot
def autolabel(rects, ax, str_format):
	######################################################
	### Function: Attach a text label above each bar in *rects*, displaying its height
	### source: https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/barchart.html
	### input: 
		### rects: output from plt.bar
		### ax
		### str_format: string format. e.g. '{:.2f}'
	### output: manipulate image directly.
	######################################################

	for rect in rects:
		height = rect.get_height()
		if(height > 0):
			ax.annotate(str_format.format(height),
						xy=(rect.get_x() + rect.get_width() / 2, height),
						xytext=(0, 3),  # 3 points vertical offset
						textcoords="offset points",
						ha='center', va='bottom')

### pct settings when plotting pie plot.
def my_autopct(pct):
	######################################################
	### Function: plot .1f% if percent > 2%
	### Adapted from: https://stackoverflow.com/questions/6170246/how-do-i-use-matplotlib-autopct
	######################################################
	return ('%1.1f%%' % pct) if pct > 2 else ''

### getting colors from cmap
def get_colors_from_cmap(cmap_name, n_group, cmap_type, **kwarg):
    ### params
    if('alpha' in kwarg.keys()):
        alpha = kwarg['alpha']
    else:
        alpha = 1
    if('return_type' in kwarg.keys()):
        return_type = kwarg['return_type']
    else:
        return_type = 'rgba'

    ### getting colors in rgba tuples
    cmap = plt.get_cmap(cmap_name)
    if(cmap_type == 'Gradient'):
        colors = []
        gradient = np.linspace(0, 1, n_group)
        for g in gradient:
            rgba = cmap(g)
            if(alpha != 1):
                rgba = list(rgba)
                rgba[3] = alpha
                rgba = tuple(rgba)
            colors.append(rgba)
    elif(cmap_type == 'Qualitative'):
        colors = []
        for i in range(0,n_group):
            rgba = cmap(i)
            if(alpha != 1):
                rgba = list(rgba)
                rgba[3] = alpha
                rgba = tuple(rgba)
            colors.append(rgba)
        if(len(set(colors)) < n_group):
            print("ERROR! Number of groups too big. Choose another colormap.")

    ### converting colors to desired return format
    if(return_type == 'rgba'):
        return colors
    elif(return_type == 'hex'):
        color_hex = []
        for color in colors:
            color_hex.append(matplotlib.colors.to_hex(color, keep_alpha=True))
        return color_hex

### getting colors that seperate the most based on number of colors needed
def get_colors(n_group, **kwarg):
	######################################################
	### Function: Return colors based on number of colors and colormap chosen by "choose_cmap" function
	### input: 
		### n_group(int): number of colors
		### kwarg:
			###'return_type': indicate data type of colors for return. 'hex' for string of hex codes; 'rgba' for numpy array ([r,g,b,a] for each color)
			###'is_last_grey': indicate whether to set the last color to grey.
	### output: colors in list of strings or numpy array.
	######################################################
	### decompose parameters
	if('return_type' in kwarg.keys()):
		return_type = kwarg['return_type']
	else:
		return_type = 'rgba'
		
	### n<=8, colormap = Set2
	if(n_group <= 8):
		cmap_list = pl.cm.Set2(np.linspace(0,1,8))
		colors = cmap_list[:n_group]
	### 8<n<=12, colormap = Set3 w/ grey at last
	elif(n_group > 8 and n_group <= 12):
		cmap_list = pl.cm.Set3(np.linspace(0,1,12))
		cmap_list[[8, 11]] = cmap_list[[11, 8]]
		colors = cmap_list[:n_group]
	### 12<n<=20, colormap = tab20 w/ greys at last
	elif(n_group > 12 and n_group <= 20):
		cmap_list = pl.cm.tab20(np.linspace(0,1,20))
		cmap_list[[14, 18]] = cmap_list[[18, 14]]
		cmap_list[[15, 19]] = cmap_list[[19, 15]]
		colors = cmap_list[:n_group]
	### n>20, colormap = gist_ncar
	else:
		cmap = plt.get_cmap('gist_ncar')
		gradient = np.linspace(0, 1, n_group)
		colors = []
		for g in gradient:
			colors.append(cmap(g))
		colors[-1] = [0.7, 0.7, 0.7, 1] # make the last color grey
	if('is_last_grey' in kwarg.keys()):
		if(kwarg['is_last_grey']):
			colors[-1] = [0.7, 0.7, 0.7, 1]
	if(return_type == 'rgba'):
		return colors
	elif(return_type == 'hex'):
		color_hex = []
		for color in colors:
			color_hex.append(matplotlib.colors.to_hex(color, keep_alpha=True))
		return color_hex

"""plot_region: get summary plots of a particular region"""
def get_logistic_params(t, pt, **kwarg):
	### params
	if('maxfev' in kwarg.keys()):
		maxfev = kwarg['maxfev']
	else:
		maxfev = 100000
	if('bounds' in kwarg.keys()):
		bounds = kwarg['bounds']
	if('plot_range' in kwarg.keys()):
		plot_range = kwarg['plot_range']
	else:
		plot_range = True
	if('method' in kwarg.keys()):
		method = kwarg['method']
	else:
		if('bounds' in kwarg.keys()):
			method = 'trf'
		else:
			method = 'lm'
	if('p0' in kwarg.keys()):
		p0 = kwarg['p0']
	else:
		p0 = (1,1,1)
	fit_gap = 30

	if(plot_range):
		num_fits = len(np.arange(20, len(t), fit_gap)) + 1
		popt_log = np.zeros((num_fits,3))
		r2 = np.zeros(num_fits)
		t0s = np.zeros(num_fits)
	else:
		popt_log = np.zeros((1,3))
		r2 = np.zeros(1)
		t0s = np.zeros(1)
	# if('p0' in kwarg.keys()):
	if(plot_range):
		if('bounds' in kwarg.keys()):
			popt_log[0,:], pcov_log = opt.curve_fit(my_func.logistic_growth,  t,  pt, p0 = p0, maxfev = maxfev, bounds = bounds, method = method)
			t0s[0] = t[0]
			r2[0] = my_func.get_r_squared(t, pt, popt_log[0,:], 'logistic_growth')
			for i in range(1, num_fits):
				len_t = np.arange(20, len(t), fit_gap)[i-1]
				t_new = np.arange(len_t)
				pt_new = pt[-len_t:]
				popt_log[i,:], pcov_log = opt.curve_fit(my_func.logistic_growth,  t_new,  pt_new, p0 = p0, maxfev = maxfev, bounds = bounds, method = method)
				t0s[i] = t[-len_t]
				r2[i] = my_func.get_r_squared(t_new, pt_new, popt_log[i,:], 'logistic_growth')
		else:
			popt_log[0,:], pcov_log = opt.curve_fit(my_func.logistic_growth,  t,  pt, p0 = p0, maxfev = maxfev, method = method)
			t0s[0] = t[0]
			r2[0] = my_func.get_r_squared(t, pt, popt_log[0,:], 'logistic_growth')
			for i in range(1, num_fits):
				len_t = np.arange(20, len(t), fit_gap)[i-1]
				t_new = np.arange(len_t)
				pt_new = pt[-len_t:]
				popt_log[i,:], pcov_log = opt.curve_fit(my_func.logistic_growth,  t_new,  pt_new, p0 = p0, maxfev = maxfev, method = method)
				t0s[i] = t[-len_t]
				r2[i] = my_func.get_r_squared(t_new, pt_new, popt_log[i,:], 'logistic_growth')
		# print(popt_log, r2, t0s)
		for i in range(1, len(r2)):
			if(r2[i] < 0.98):
				popt_log[i,:] = np.inf
				r2[i] = np.inf
				t0s[i] = np.inf
		# print(popt_log, r2, t0s)
	else:
		if('bounds' in kwarg.keys()):
			popt_log[0,:], pcov_log = opt.curve_fit(my_func.logistic_growth,  t,  pt, p0 = kwarg['p0'], maxfev = maxfev, bounds = bounds, method = method)
		else:
			popt_log[0,:], pcov_log = opt.curve_fit(my_func.logistic_growth,  t,  pt, p0 = kwarg['p0'], maxfev = maxfev, method = method)
		
		r2 = my_func.get_r_squared(t, pt, popt_log[0,:], 'logistic_growth')
		t0s[0] = t[0]
	popt_log_final = popt_log[~np.all(popt_log == np.inf, axis=1)]
	r2_final = r2[r2 != np.inf]
	t0_final = t0s[t0s != np.inf]
	# print(popt_log_final, r2_final, t0_final)
	return popt_log_final, r2_final, t0_final

def plot_predictions(ax1, tx0, y0, dy0, x1, tx1, popt_logs, scale_factor, color, plot_range):
	num_plots = popt_logs.shape[0]	
	if(plot_range):
		i_range = np.arange(num_plots)
		# print(i_range)
	else:
		i_range = [0]
	ys = np.zeros((x1.shape[1], num_plots))
	# df = pd.DataFrame(columns = ['x', 'y'])
	for i in i_range:
		# df_tmp = pd.DataFrame(columns = ['x', 'y'])
		# df_tmp['x'] = tx1
		# a = my_func.logistic_growth(x1[i,:], *popt_logs[i,:]) / scale_factor
		# df_tmp['y'] = my_func.logistic_growth(x1[i,:], *popt_logs[i,:]) / scale_factor
		ys[:,i] = my_func.logistic_growth(x1[i,:], *popt_logs[i,:]) / scale_factor
		# df = df.append(df_tmp)
	
	sns.set(style = 'ticks', rc={"lines.linewidth": 2})
	ax2 = ax1.twinx()
	colors = get_colors_from_cmap('Greys', num_plots+1, 'Gradient', alpha = 0.5)
	for i in i_range:
		ax1.bar(tx1[1:], ys[1:,i] - ys[:-1, i], color = colors[i+1], width = 1)
	ax1.plot(tx0, dy0, '--', color = color)
	
	ax2.plot(tx0, y0, '.', color = color)
	colors = get_colors_from_cmap('Greys', num_plots+2, 'Gradient', alpha = 0.8)
	for i in i_range:
		ax2.plot(tx1, ys[:,i], color = colors[i+1], linewidth = 3)
	ax2.plot(tx0, y0, '.', color = color)
	# sns.lineplot(x='x', y='y', data = df, ax = ax2, color = color)
	ax1.set_ylim(bottom = 0)
	ax2.set_ylim(bottom = 0)
	y_ends = ys[-1,:]
	ind_midds = np.zeros((1,num_plots))-1
	ind_maxes = np.zeros((1,num_plots))-1
	for i in i_range:
		ind_midds[0,i] = int(list(ys[1:,i] - ys[:-1, i]).index(max(ys[1:,i] - ys[:-1, i])
			))
		if(len(np.argwhere(np.abs(np.diff(ys[int(ind_midds[0,i]):,i]) < 1)))):
			ind_maxes[0,i] = np.argwhere(np.abs(np.diff(ys[int(ind_midds[0,i]):,i]) < 1))[0] + ind_midds[0,i]
	ax1.set_xlabel('Time')
	ax2.set_ylabel('Total')
	ax1.set_ylabel('Daily')
	ax2.legend(['Total Data', 'Logistic Fit'], loc = 'upper left')
	ax1.legend(['Daily Data', 'Daily Fitted'], loc = 'center left')
	return ax1, ax2, y_ends, ind_midds, ind_maxes

def plot_region(df_region, region_name, **kwarg):
	######################################################
	### Function: plot time_series, stats, and fit information of a time_series data of a particular region
	### Input:
		### df_region(dataframe): output from reshape_dataframe. index = datetime
		### region_name(string): name of region
		### additional params
			### is_fitting (True/False): default = False
			### fitting_params (dic)
				# future = 0
				# p0_exp = (1,1)
				# p0_log = (1,1,1)
				# maxfev = 100000
				# bounds_c = (-np.inf, np.inf)
				# bounds_d = (-np.inf, np.inf)
				# death_to_confirmed = False
				# CFR = df_region.CFR[-1]/100
				# thr_c = "rolling", "total", "int(ind)"
				# thr_d = "rolling", "total", "int(ind)"
			### plotting_params (dic): default as below.
				# 'figsize':(15, 15),
				# 'time_series_cols' : ['Confirmed', 'Deaths', 'Recovered', 'Active'],
				# 'locator_param' : 4,
				# 'locator_param_future':8,
				# 'num_of_rols' : 6,
				# 'cat_color' : {'Confirmed':'tab:blue',
				#                 'Deaths':'tab:orange', 
				#                 'Recovered':'tab:green', 
				#                 'Active':'tab:red'}
	### Output: Figure. Total, Daily, Growth Factor, CFR, confirmed/death fits
	######################################################
	######### unravel params
	if('is_fitting' in kwarg.keys()):
		is_fitting = kwarg['is_fitting']
		if('fitting_params' in kwarg.keys()):
			fitting_params = kwarg['fitting_params']
		else:
			fitting_params = {}
	else:
		is_fitting = False

	if('show_recent' in kwarg.keys()):
		show_recent = kwarg['show_recent']
	else:
		show_recent = False

	if('plotting_params' in kwarg.keys()):
		plotting_params = kwarg['plotting_params']
	else:
		cat_color = {'Confirmed':'tab:blue',
					'Deaths':'tab:red',
					'Recovered':'tab:green', 
					'Active':'tab:orange'}
		plotting_params = {
			'figsize':(15, 15),
			'time_series_cols' : ['Confirmed', 'Deaths', 'Recovered', 'Active'],
			'locator_param' : 4,
			'locator_param_future':8,
			'num_of_rols' : 6,
			'cat_color' : cat_color
		}
	time_datetime = list(df_region.index)

	######### figure
	fig = plt.figure(figsize = plotting_params['figsize'], constrained_layout = True, facecolor = "1")
	gs = fig.add_gridspec(plotting_params['num_of_rols'], 2)

	### font size setting 
	plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
	plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
	plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
	plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('legend', fontsize=MEDIUM_SIZE)    # legend fontsize
	plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title


	### time series plot 
	ax = fig.add_subplot(gs[0, 0])
	ax2 = ax.twinx()
	legend1 = []
	legend2 = []
	
	max_y2 = 0
	for cat in plotting_params['time_series_cols']:
		if(show_recent == True):
			if(np.max(df_region.loc[time_datetime[-20:],cat]) < (np.max(df_region.loc[time_datetime[-20:],'Confirmed']))/5):
				ax2.plot(df_region.loc[:,cat], color = plotting_params['cat_color'][cat])
				if(max_y2 < np.max(df_region.loc[time_datetime[-20:],cat])):
					max_y2 = np.max(df_region.loc[time_datetime[-20:],cat])
				legend2.append(f'{cat}: {df_region.loc[time_datetime[-1], cat]:,}')
			else:
				ax.plot(df_region.loc[:,cat], color = plotting_params['cat_color'][cat])
				legend1.append(f'{cat}: {df_region.loc[time_datetime[-1], cat]:,}')
			max_y1 = np.max(df_region.loc[time_datetime[-20:],'Confirmed'])
		else:
			if(np.max(df_region.loc[:,cat])< np.max(df_region.loc[:,'Confirmed'])/5):
				ax2.plot(df_region.loc[:,cat], color = plotting_params['cat_color'][cat])
				if(max_y2 < np.max(df_region.loc[:,cat])):
					max_y2 = np.max(df_region.loc[:,cat])
				legend2.append(f'{cat}: {df_region.loc[time_datetime[-1], cat]:,}')
			else:
				ax.plot(df_region.loc[:,cat], color = plotting_params['cat_color'][cat])
				legend1.append(f'{cat}: {df_region.loc[time_datetime[-1], cat]:,}')
			max_y1 = df_region.loc[:,'Confirmed']
			
	ax.legend(legend1, loc = 'upper left', title = 'left y', title_fontsize = 12)
	ax2.legend(legend2, loc = 'center left', title = 'right y', title_fontsize = 12)
	ax2.set_ylim([0, max_y2*1.2])
	ax.set_ylim(bottom = 0)
	
	myLocator = mticker.MultipleLocator(plotting_params['locator_param'])
	ax.xaxis.set_major_locator(myLocator)
	ax.tick_params(axis = 'x', labelrotation = 45)

	ax.set_title(f'{region_name} up to {time_datetime[-1].date()}')


	### daily cases 
	ax = fig.add_subplot(gs[0, 1])
	df_difs = df_region.loc[:,plotting_params['time_series_cols']].diff()
	ax2 = ax.twinx()
	legend1 = []
	legend2 = []
	
	max_y2 = 0
	for cat in plotting_params['time_series_cols']:
		if(show_recent == True):
			if(np.max(df_difs.loc[time_datetime[-20:],cat]) < np.max(df_difs.loc[time_datetime[-20:],'Confirmed'])/5):
				ax2.plot(df_difs.loc[:,cat], color = plotting_params['cat_color'][cat])
				if(max_y2 < np.max(df_difs.loc[time_datetime[-20:],cat])):
					max_y2 = np.max(df_difs.loc[time_datetime[-20:],cat])
				legend2.append(f'{cat}: {df_difs.loc[time_datetime[-1], cat]:,}')
			else:
				ax.plot(df_difs.loc[:,cat], color = plotting_params['cat_color'][cat])
				legend1.append(f'{cat}: {df_difs.loc[time_datetime[-1], cat]:,}')
			max_y1 = np.max(df_difs.loc[time_datetime[-20:],'Confirmed'])
		else:
			if(np.max(df_difs.loc[:,cat]) < np.max(df_difs.loc[:,'Confirmed'])/5):
				ax2.plot(df_difs.loc[:,cat], color = plotting_params['cat_color'][cat])
				if(max_y2 < np.max(df_difs.loc[:,cat])):
					max_y2 = np.max(df_difs.loc[:,cat])
				legend2.append(f'{cat}: {df_difs.loc[time_datetime[-1], cat]:,}')
			else:
				ax.plot(df_difs.loc[:,cat], color = plotting_params['cat_color'][cat])
				legend1.append(f'{cat}: {df_difs.loc[time_datetime[-1], cat]:,}')
			max_y1 = np.max(df_difs.loc[:,'Confirmed'])
		
	
	ax.legend(legend1, loc = 'upper left', title = 'left y', title_fontsize = 12)
	ax2.legend(legend2, loc = 'center left', title = 'right y', title_fontsize = 12)
	ax.set_ylim(top = max_y1*1.05)
	ax2.set_ylim(top = max_y2*1.2)
	# ax.set_ylim(bottom = 0)

	myLocator = mticker.MultipleLocator(plotting_params['locator_param'])
	ax.xaxis.set_major_locator(myLocator)
	ax.tick_params(axis = 'x', labelrotation = 45)

	ax.set_title(f'{region_name} Daily Cases up to {time_datetime[-1].date()}')


	### growth factor 
	ax = fig.add_subplot(gs[1, 0])
	x = my_func.get_datetime_arange(time_datetime[0] - timedelta(days=2), len(time_datetime)+3)
	ax.plot(df_region.GFc_rolling, color = plotting_params['cat_color']['Confirmed'])
	ax.plot(df_region.GFd_rolling, color = plotting_params['cat_color']['Deaths'])
	ax.plot(x, np.full(len(x), 1), '--', color = 'k')
	
	ax.legend(['Confirmed', 'Deaths', 'y=1'])
	ax.set_title(f'{region_name} Growth Factor: {df_region.GFc_rolling[-1]:.2f}/{df_region.GFd_rolling[-1]:.2f} at {time_datetime[-1].date()}')
	ax.set_ylim([0, min(2.5, max(np.max(df_region.GFc_rolling) + 0.5, np.max(df_region.GFd_rolling) + 0.5))])
	
	ax.set_xlim([time_datetime[0], time_datetime[-1] + timedelta(days=2)])
	
	myLocator = mticker.MultipleLocator(plotting_params['locator_param'])
	ax.xaxis.set_major_locator(myLocator)
	ax.tick_params(axis = 'x', labelrotation = 45)

	### closed cases
	ax = fig.add_subplot(gs[1, 1])
	if('Recovered' in plotting_params['time_series_cols']):
		ax.plot(df_region.Recovered_prop, color = plotting_params['cat_color']['Recovered'])
		ax.plot(df_region.Death_prop, color = plotting_params['cat_color']['Deaths'])
		ax2 = ax.twinx()
		ax2.plot(df_region.CFR, color = 'grey')
		ax.legend(['Recovered%', 'Deaths%'], loc = 'upper left')
		ax2.legend(['CFR'], loc = 'center right')
		ax.set_ylabel('Closed Cases')
		ax2.set_ylabel('CFR')
		ax.set_title(f'{region_name} Closed Cases Deaths% = {df_region.Death_prop[-1]:.1f}%; CFR = {df_region.CFR[-1]:.1f}%')
	else:
		ax.plot(df_region.CFR, color = 'grey')
		ax.set_title(f'{region_name} CFR = {df_region.CFR[-1]:.1f}%')
	
	myLocator = mticker.MultipleLocator(plotting_params['locator_param'])
	ax.xaxis.set_major_locator(myLocator)
	ax.tick_params(axis = 'x', labelrotation = 45)

	### fitting
	if(is_fitting == True):
		### fitting params
		if('future' in fitting_params.keys()):
			future = fitting_params['future']
		else:
			future = 0
		if('p0_exp' in fitting_params.keys()):
			p0_exp = fitting_params['p0_exp']
		else:
			p0_exp = (1,1)
		if('p0_log' in fitting_params.keys()):
			p0_log = fitting_params['p0_log']
		else:
			p0_log = (1,1,1)
		if('maxfev' in fitting_params.keys()):
			maxfev = fitting_params['maxfev']
		else:
			maxfev = 100000
		if('bounds_c' in fitting_params.keys()):
			bounds_c = fitting_params['bounds_c']
		else:
			bounds_c = (-np.inf, np.inf)
		if('bounds_d' in fitting_params.keys()):
			bounds_d = fitting_params['bounds_d']
		else:
			bounds_d = (-np.inf, np.inf)
		if('method_c' in fitting_params.keys()):
			method_c = fitting_params['method_c']
		else:
			if('bounds_c' in fitting_params.keys()):
				method_c = 'trf'
			else:
				method_c = 'lm'
		if('method_d' in fitting_params.keys()):
			method_d = fitting_params['method_d']
		else:
			if('bounds_d' in fitting_params.keys()):
				method_d = 'trf'
			else:
				method_d = 'lm'
		if('death_to_confirmed' in fitting_params.keys()):
			death_to_confirmed = fitting_params['death_to_confirmed']
			if('CFR' in fitting_params.keys()):
				cfr = fitting_params['CFR']
			else:
				cfr = df_region.CFR[-1]/100
		else:
			death_to_confirmed = False
		if('plot_range' in fitting_params.keys()):
			plot_range = fitting_params['plot_range']
		else:
			plot_range = True

		### fitting data
		y_c = df_region.Confirmed.to_numpy()
		y_d = df_region.Deaths.to_numpy()
		
		if('thr_c' in fitting_params.keys()):
			if(np.char.isnumeric(fitting_params['thr_c'])):
				ind_tc = int(fitting_params['thr_c'])
			elif(fitting_params['thr_c'] == 'total'):
				res_c = df_region[df_region.Confirmed > 100].index[0]
				ind_tc = min(max(0, time_datetime.index(res_c)), len(time_datetime)-20)
			elif(fitting_params['thr_c'] == 'rolling'):
				res_c = df_region[df_region.GFc_thr != 0.0].bfill(axis=1).index[0]
				ind_tc = min(max(0, time_datetime.index(res_c)-3), len(time_datetime)-20)
		else:
			res_c = df_region[df_region.GFc_thr != 0.0].bfill(axis=1).index[0]
			ind_tc = min(max(0, time_datetime.index(res_c)-3), len(time_datetime)-20)
		
		if('thr_d' in fitting_params.keys()):
			if(np.char.isnumeric(fitting_params['thr_d'])):
				ind_td = int(fitting_params['thr_d'])
			if(fitting_params['thr_d'] == 'total'):
				res_d = df_region[df_region.Deaths > 10].index[0]
				ind_td = min(max(0, time_datetime.index(res_d)), len(time_datetime)-20)
			elif(fitting_params['thr_d'] == 'rolling'):
				res_d = df_region[df_region.GFd_thr != 0.0].bfill(axis=1).index[0]
				ind_td = min(max(0, time_datetime.index(res_d)-3), len(time_datetime)-20)
		else:
			ind_td = ind_tc
		# print(ind_tc, ind_td)
		
		tc = np.arange(len(time_datetime))[ind_tc:] - ind_tc
		td = np.arange(len(time_datetime))[ind_td:] - ind_td
		pt_c = y_c[ind_tc:]
		pt_d = y_d[ind_td:]

		if('bounds_c' in fitting_params.keys()):
			popt_exp_c, _ = opt.curve_fit(my_func.exp_growth,  tc,  pt_c, p0 = p0_exp, maxfev = maxfev)
			r2_exp_c = my_func.get_r_squared(tc, pt_c, popt_exp_c, 'exp_growth')
			if(future != 0):
				popt_logs_c, r2_logs_c, t0s_c = get_logistic_params(tc, pt_c, p0 = p0_log, maxfev = maxfev, bounds = bounds_c, plot_range = plot_range, method = method_c)
			else:
				popt_logs_c, _ = opt.curve_fit(my_func.logistic_growth,  tc,  pt_c, p0 = p0_log, maxfev = maxfev, bounds = bounds_c, method = method_c)
				r2_logs_c = my_func.get_r_squared(tc, pt_c, popt_logs_c, 'logistic_growth')
		else:
			if(future != 0):
				popt_logs_c, r2_logs_c, t0s_c = get_logistic_params(tc, pt_c, p0 = p0_log, maxfev = maxfev, plot_range = plot_range, method = method_c)
			else:
				popt_logs_c, _ = opt.curve_fit(my_func.logistic_growth,  tc,  pt_c, p0 = p0_log, maxfev = maxfev, method = method_c)
				r2_logs_c = my_func.get_r_squared(tc, pt_c, popt_logs_c, 'logistic_growth')

		if('bounds_d' in fitting_params.keys()):
			popt_exp_d, _ = opt.curve_fit(my_func.exp_growth,  td,  pt_d, p0 = p0_exp, maxfev = maxfev)
			r2_exp_d = my_func.get_r_squared(td, pt_d, popt_exp_d, 'exp_growth')
			if(future != 0):
				popt_logs_d, r2_logs_d, t0s_d = get_logistic_params(td, pt_d, p0 = p0_log, maxfev = maxfev, bounds = bounds_d, plot_range = plot_range, method = method_d)
			else:
				popt_logs_d, _ = opt.curve_fit(my_func.logistic_growth,  td,  pt_d, p0 = p0_log, maxfev = maxfev, bounds = bounds_d, method = method_d)
				r2_logs_d = my_func.get_r_squared(td, pt_d, popt_logs_d, 'logistic_growth')
		else:
			if(future != 0):
				popt_logs_d, r2_logs_d, t0s_d = get_logistic_params(td, pt_d, p0 = p0_log, maxfev = maxfev, plot_range = plot_range, method = method_d)
			else:
				popt_logs_d, _ = opt.curve_fit(my_func.logistic_growth,  td,  pt_d, p0 = p0_log, maxfev = maxfev, method = method_d)
				r2_logs_d = my_func.get_r_squared(td, pt_d, popt_logs_d, 'logistic_growth')			
		# print(popt_logs_c, popt_logs_d, t0s_c, t0s_d)
		# print(t0s_c.shape, t0s_d.shape)

		### confirmed fitting plot
		ax = fig.add_subplot(gs[2, 0])
		x = np.arange(len(time_datetime))
		tx = my_func.get_datetime_arange(time_datetime[0], len(time_datetime))
		tx1 = my_func.get_datetime_arange(time_datetime[0], len(time_datetime) + future)

		if(future == 0):
			x1 = np.arange(len(time_datetime) + future) - ind_tc
			ax.plot(tx, y_c, '.', ms = 10, color = plotting_params['cat_color']['Confirmed'])
			ax.plot(tx, my_func.exp_growth(x1, *popt_exp_c), '--', color = plotting_params['cat_color']['Confirmed'])
			ax.plot(tx, my_func. logistic_growth(x1, *popt_logs_c), color = plotting_params['cat_color']['Confirmed'])
			ax.set_ylim([0, np.ceil(y_c[-1] + y_c[-1]/10)])
		else:
			if(len(t0s_c) != 0):
				x1 = np.zeros((len(t0s_c),len(time_datetime) + future))
				for i in range(len(t0s_c)):
					x1[i,:] = np.arange(len(time_datetime) + future) -  t0s_c[i] - ind_tc
				# print(x1)
				dy_c = df_region.Daily_Confirmed_smoothed
				ax1, ax2, y_ends, ind_midds, ind_maxes = plot_predictions(ax, tx, y_c, dy_c, x1, tx1, popt_logs_c, 1, plotting_params['cat_color']['Confirmed'], plot_range = plot_range)
				date_midds = []
				date_maxs = []
				for k in [0,1]:
					if(k == 0):
						ind_mid = int(np.min(ind_midds))
						ind_max = int(np.min(ind_maxes))
					else:
						ind_mid = int(np.max(ind_midds))
						ind_max = int(np.max(ind_maxes))
					date_midds.append(time_datetime[ind_tc] + timedelta(days=ind_mid))
					date_maxs.append(time_datetime[ind_tc] + timedelta(days=ind_max))
				confirmed_total = np.mean(y_ends)
			else:
				ax.plot(tx, y_c, '.', ms = 10, color = plotting_params['cat_color']['Confirmed'])
				dy_c = df_region.Daily_Confirmed_smoothed
				ax.plot(tx, dy_c, '--', color = plotting_params['cat_color']['Confirmed'])

		if(future == 0):
			ax.legend(['Confirmed', 
						f'Exp Fit: R2 = {r2_exp_c:.2f}', 
						f'Logistic Fit: R2 = {r2_logs_c:.2f}'])
			ax.set_title(f'{region_name} Confirmed Fit: r = {popt_exp_c[0]:.2f}/{popt_logs_c[0]:.2f}', fontsize = 14)
		else:
			if(len(t0s_c) != 0):
				if(plot_range):
					ax.set_title(f'{region_name} Confirmed Prediction: K = {np.min(y_ends):,.0f}~{np.max(y_ends):,.0f}', fontsize = 14)
				else:
					ax.set_title(f'{region_name} Confirmed Prediction: K = {np.max(y_ends):,.0f}', fontsize = 14)
				pp = f'{region_name} Confirmed cases: max daily increase at {df_region[df_region.Daily_Confirmed_smoothed == np.max(df_region.Daily_Confirmed_smoothed)].index[-1].date()}, '
				if(df_region.GFc_rolling[-1]<1):
					pp += f'new cases decreasing since {(df_region[df_region.GFc_rolling>1].index[-1] + timedelta(days=1)).date()}. '
				else:
					pp += f'GF > 1 for today ({time_datetime[-1].date()}). '
				pp += f'Prediction: r={np.mean(popt_logs_c[:,0]):.2f}, '
				if(plot_range):
					pp += f'K = {np.min(y_ends):,.0f}~{np.max(y_ends):,.0f}, '
					pp += f'R^2 = {np.max(r2_logs_c):.2f} ~ {np.min(r2_logs_c):.2f}; '
					pp += f'Predicted peak increase at {date_midds[0].date()} ~ {date_midds[1].date()}, '
				else:
					pp += f'K = {np.max(y_ends):,.0f}, '
					pp += f'R^2 = {r2_logs_c[0]:.2f}; '
					pp += f'Predicted peak increase at {date_midds[1].date()}, '
				if(np.max(ind_maxes) == -1):
					pp += f'max will not be reached by {(time_datetime[-1] + timedelta(days=future)).date()}.'
				else:
					if(-1 not in ind_maxes):
						pp += f'max will be reached at {date_maxs[0].date()} ~ {date_maxs[1].date()}.'
					else:
						pp += f'max will be reached at {date_maxs[1].date()}.'
				print(pp)
			else:
				print("no valid prediction!")
				ax.set_title(f'{region_name} Confirmed', fontsize = 14)
		
		myLocator = mticker.MultipleLocator(plotting_params['locator_param_future'])
		ax.xaxis.set_major_locator(myLocator)
		ax.tick_params(axis = 'x', labelrotation = 45)
		ax.set_xlim(left = time_datetime[0])


		### death fitting plot
		ax = fig.add_subplot(gs[2, 1])
		x = np.arange(len(time_datetime))
		tx = my_func.get_datetime_arange(time_datetime[0], len(time_datetime))
		tx1 = my_func.get_datetime_arange(time_datetime[0], len(time_datetime) + future)
		
		if(y_d[-1] <= 50):
			ax.plot(tx, y_d, '.', ms = 10, color = plotting_params['cat_color']['Deaths'])
			ax.set_title(f'{region_name} Deaths, total = {y_d[-1]} too low to fit')
		else:
			if(future == 0):
				x1 = np.arange(len(time_datetime) + future) - ind_td
				ax.plot(tx, y_d, '.', ms = 10, color = plotting_params['cat_color']['Deaths'])
				ax.plot(tx1, my_func.exp_growth(x1, *popt_exp_d), '--', color = plotting_params['cat_color']['Deaths'])
				ax.plot(tx1, my_func. logistic_growth(x1, *popt_logs_d), color = plotting_params['cat_color']['Deaths'])
				ax.set_ylim([0, np.ceil(y_d[-1] + y_d[-1]/10)])
			else:
				if(len(t0s_d) != 0):
					x1 = np.zeros((len(t0s_d),len(time_datetime) + future))
					for i in range(len(t0s_d)):
						x1[i,:] = np.arange(len(time_datetime) + future) -  t0s_d[i] - ind_td
					dy_d = df_region.Daily_Deaths_smoothed
					ax1, ax2, y_ends, ind_midds, ind_maxes = plot_predictions(ax, tx, y_d, dy_d, x1, tx1, popt_logs_d, 1, plotting_params['cat_color']['Deaths'], plot_range = plot_range)
					date_midds = []
					date_maxs = []
					for k in [0,1]:
						if(k == 0):
							ind_mid = int(np.min(ind_midds))
							ind_max = int(np.min(ind_maxes))
						else:
							ind_mid = int(np.max(ind_midds))
							ind_max = int(np.max(ind_maxes))
						date_midds.append(time_datetime[ind_td] + timedelta(days=ind_mid))
						date_maxs.append(time_datetime[ind_td] + timedelta(days=ind_max))
					# print(date_max)
					deaths_total = np.mean(y_ends)
				else:
					ax.plot(tx, y_d, '.', ms = 10, color = plotting_params['cat_color']['Deaths'])
					dy_d = df_region.Daily_Deaths_smoothed
					ax.plot(tx, dy_d, '--', color = plotting_params['cat_color']['Deaths'])
			if(future == 0):
				ax.legend(['Deaths', 
							f'Exp Fit: R2 = {r2_exp_d:.2f}', 
							f'Logistic Fit: R2 = {r2_logs_d:.2f}'])
				ax.set_title(f'{region_name} Deaths Fit: r = {popt_exp_d[0]:.2f}/{popt_exp_d[0]:.2f}', fontsize = 18)
			else:
				if(len(t0s_d) != 0):
					if(plot_range):
						ax.set_title(f'{region_name} Deaths Prediction: K = {np.min(y_ends):,.0f}~{np.max(y_ends):,.0f}', fontsize = 14)
					else:
						ax.set_title(f'{region_name} Deaths Prediction: K = {np.max(y_ends):,.0f}', fontsize = 14)
					pp = f'{region_name} Deaths: max daily increase at {df_region[df_region.Daily_Deaths_smoothed == np.max(df_region.Daily_Deaths_smoothed)].index[-1].date()}, '
					if(df_region.GFd_rolling[-1]<1):
						pp += f'new cases decreasing since {(df_region[df_region.GFd_rolling>1].index[-1] + timedelta(days=1)).date()}. '
					else:
						pp += f'GF > 1 for today ({time_datetime[-1].date()}). '

					pp += f'Prediction: r={np.mean(popt_logs_d[:,0]):.2f}, '
					if(plot_range):
						pp += f'K = {np.min(y_ends):,.0f}~{np.max(y_ends):,.0f}, '
						pp += f'R^2 = {np.max(r2_logs_d):.2f} ~ {np.min(r2_logs_d):.2f}; '
						pp += f'peak increase at {date_midds[0].date()} ~ {date_midds[1].date()}, '
					else:
						pp += f'K = {np.max(y_ends):,.0f}, '
						pp += f'R^2 = {r2_logs_d[0]:.2f}, '
						pp += f'Predicted peak increase at {date_midds[1].date()}, '
					pp += f'CFR = {deaths_total/confirmed_total*100:.2f}%; '
					if(np.max(ind_maxes) == -1):
						pp += f'max will not be reached by {(time_datetime[-1] + timedelta(days=future)).date()}.'
					else:
						if(-1 not in ind_maxes):
							pp += f'max will be reached at {date_maxs[0].date()} ~ {date_maxs[1].date()}.'
						else:
							pp += f'max will be reached at {date_maxs[1].date()}.'
					print(pp)
				else:
					print("no valid prediction!")
					ax.set_title(f'{region_name} Deaths', fontsize = 14)

				
		myLocator = mticker.MultipleLocator(plotting_params['locator_param_future'])
		ax.xaxis.set_major_locator(myLocator)
		ax.tick_params(axis = 'x', labelrotation = 45)
		ax.set_xlim(left = time_datetime[0])
		
		### confirmed prediction based on death and current CFR
		if(future != 0 and death_to_confirmed and y_d[-1] > 50):
			ax = fig.add_subplot(gs[3, 0])
			ax1, ax2, y_ends, ind_midds, ind_maxes = plot_predictions(ax, tx, y_c, dy_c, x1, tx1, popt_logs_d, cfr, plotting_params['cat_color']['Confirmed'], plot_range = plot_range)
			date_midds = []
			date_maxs = []
			for k in [0,1]:
				if(k == 0):
					ind_mid = int(np.min(ind_midds))
					ind_max = int(np.min(ind_maxes))
				else:
					ind_mid = int(np.max(ind_midds))
					ind_max = int(np.max(ind_maxes))
				date_midds.append(time_datetime[ind_tc] + timedelta(days=ind_mid))
				date_maxs.append(time_datetime[ind_tc] + timedelta(days=ind_max))
			confirmed_total = np.mean(y_ends)
			
			if(plot_range):
				ax.set_title(f'{region_name} Confirmed projection (based on CFR = {cfr*100:.1f}%)\nK = {np.min(y_ends):,.0f}~{np.max(y_ends):,.0f}')
			else:
				ax.set_title(f'{region_name} Confirmed projection (based on CFR = {cfr*100:.1f}%)\nK = {np.max(y_ends):,.0f}')
			pp = f'{region_name} Confirmed Prediction (based on CFR = {cfr*100:.1f}%): '
			if(plot_range):
				pp += f'K = {np.min(y_ends):,.0f}~{np.max(y_ends):,.0f}, '
				pp += f'peak increase at {date_midds[0].date()} ~ {date_midds[1].date()}, '

			else:
				pp += f'K = {np.max(y_ends):,.0f}, '
				pp += f'peak increase at {date_midds[1].date()}, '
			if(np.max(ind_maxes) == -1):
				pp += f'max will not be reached by {(time_datetime[-1] + timedelta(days=future)).date()}.'
			else:
				if(-1 not in ind_maxes):
					pp += f'max will be reached at {date_maxs[0].date()} ~ {date_maxs[1].date()}.'
				else:
					pp += f'max will be reached at {date_maxs[1].date()}.'
			# print(pp)
			print(popt_logs_c, popt_logs_d, popt_exp_c, popt_exp_d)

			myLocator = mticker.MultipleLocator(plotting_params['locator_param_future'])
			ax.xaxis.set_major_locator(myLocator)
			ax.tick_params(axis = 'x', labelrotation = 45)
			ax.set_xlim(left = time_datetime[0])



"""Pie charts by continent"""
def plot_continents(df_continents):
	######################################################
	### Function: pie charts of cumulative data divided by continent.
	### Input: df_continents(DataFrame): columns = ['Total_Confirmed', 'New_Confirmed', 'Total_Deaths', 'New_Deaths','Population', 'Land Area', 'Pos_per_Million']
	### Output: Figure, with pie chart of new and total confirmed/deaths cases by continent.
	######################################################
	fig = plt.figure(figsize = (10, 6), constrained_layout=True, facecolor="1")
	gs = fig.add_gridspec(2,2)

	for i in [0,1]:
		for j in [0,1]:
			if(i == 0):
				cat = 'Total'
				plt_title = 'Total'
			else:
				cat = 'New'
				plt_title = 'New'
			if(j == 0):
				cat += '_Confirmed'
				plt_title += ' Confirmed cases by continent'
				df_plot = df_continents[cat]
			else:
				cat += '_Deaths'
				plt_title += ' Deaths by continent'
				df_plot = df_continents[cat]
			ax = fig.add_subplot(gs[j,i])

			df_plot = df_plot.sort_values(ascending=False)
			percentages = []
			labels = []
			total = np.sum(df_continents[cat])
			for cont in df_plot.index:
				percentages.append(df_plot[cont]/total*100)
				labels.append(cont)
			colors = pl.cm.Set2(np.linspace(0,1,len(percentages)))
			wedges, texts, autotexts = ax.pie(percentages, colors = colors, 
											  autopct = my_autopct, pctdistance=0.7,
											  shadow=False, startangle=90,
											  textprops = dict(size = 12))
			ax.legend(wedges, labels, loc="center right", 
					  bbox_to_anchor = (1, 0, 0.2, 1))
			ax.axis('equal')
			ax.set_title(plt_title)
	fig.subplots_adjust(left=0.0,right=0.95)

"""us tests"""
def plot_tests_bar_and_percent(ax1, x, y1, y2, color1, color2, legend1, legend2, ylabel1, ylabel2):
	ax2 = ax1.twinx()
	for i, cat in enumerate(y1.columns):
		ax1.bar(x, y1[cat], color = color1[i], alpha = 0.8)
	ax2.plot(x, y2, linewidth = 2, color = color2)
	ax1.legend(legend1, loc = 'upper left')
	ax2.legend(legend2, loc = 'upper center')
	ax1.set_ylabel(ylabel1)
	ax2.set_ylabel(ylabel2)
	return ax1, ax2

def tests_us_vs_state(df_tests_us_daily, df_tests_onestate_daily, df_us, df_st, state, **kwarg):
	if('figsize' in kwarg.keys()):
		figsize = kwarg['figsize']
	else:
		figsize = (15, 30)
	fig = plt.figure(figsize = figsize, constrained_layout=True, facecolor="1")
	gs = fig.add_gridspec(3, 2)

	plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
	plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
	plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
	plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('legend', fontsize=MEDIUM_SIZE)    # legend fontsize
	plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title


	for i in [0,1]:
		if(i == 0):
			plt_title = 'US'
			df_plot = df_us
			df_test = df_tests_us_daily
		else:
			plt_title = state
			df_plot = df_st
			df_test = df_tests_onestate_daily

		if(kwarg['plot_choice'] == 'Testing'):

			### bar graph of tests
			ax = fig.add_subplot(gs[0, i])
			x = df_test.index
			y1 = df_test.loc[:,['total', 'positive']]
			y2 = df_test.positive/df_test.total*100
			legend1 = ['Negative', 'Positive']
			legend2 = ['%positive']
			ylabel1 = 'Number of tests'
			ylabel2 = '%Positive'
			ax1, ax2 = plot_tests_bar_and_percent(ax, x, 
												  y1, y2,
												  [cat_color['Negative'], cat_color['Positive']], 'k', 
												  legend1, legend2, ylabel1, ylabel2)
			myLocator = mticker.MultipleLocator(8)
			ax.xaxis.set_major_locator(myLocator)
			ax.tick_params(axis = 'x', labelrotation = 45)
			ax.set_title(f'{plt_title} tests performed ({df_test.positive[-1]/df_test.total[-1]*100:.0f}% today)')
			if('yscales' in kwarg.keys()):
				ax.set_yscale(kwarg['yscales'][1])
			if('resent_scales' in kwarg.keys()):
				if(kwarg['resent_scales'] == True):
					ymax1 = np.max(y1.to_numpy()[-20:,:].flatten())
					ymin1 = np.min(y1.to_numpy()[-20:,:].flatten())
					ymax2 = np.max(y2.to_numpy()[-20:].flatten())
					ymin2 = np.min(y2.to_numpy()[-20:].flatten())
					ax1.set_ylim([min(0, ymin1*1.1), ymax1*1.1])
					ax2.set_ylim([min(0, ymin1*1.1), ymax2*1.1])

			### daily increase
			ax = fig.add_subplot(gs[1, i])
			y1 = df_test.loc[:,['d_total', 'd_positive']]
			y2 = df_test.d_positive/df_test.d_total*100
			ax1, ax2 = plot_tests_bar_and_percent(ax, x, 
												  y1, y2,
												  [cat_color['Negative'], cat_color['Positive']], 'k', 
												  legend1, legend2, ylabel1, ylabel2)
			myLocator = mticker.MultipleLocator(8)
			ax.xaxis.set_major_locator(myLocator)
			ax.tick_params(axis = 'x', labelrotation = 45)
			ax.set_title(f'{plt_title} daily tests performed ({df_test.d_positive[-1]/df_test.d_total[-1]*100:.0f}% today)')
			ax.set_ylabel('Number of tests')
			if('yscales' in kwarg.keys()):
				ax1.set_yscale(kwarg['yscales'][2])
			if('resent_scales' in kwarg.keys()):
				if(kwarg['resent_scales'] == True):
					ymax1 = np.max(y1.to_numpy()[-20:,:].flatten())
					ymin1 = np.min(y1.to_numpy()[-20:,:].flatten())
					ymax2 = np.max(y2.to_numpy()[-20:].flatten())
					ymin2 = np.min(y2.to_numpy()[-20:].flatten())
					ax1.set_ylim([min(0, ymin1*1.1), ymax1*1.1])
					ax2.set_ylim([min(0, ymin1*1.1), ymax2*1.1])

			### tests vs. positive
			ax = fig.add_subplot(gs[2, i])
			sns.regplot(data = df_test, x = 'positive', y = 'total')
			sns.regplot(x = df_plot.loc[df_test.index, 'Confirmed'], y = df_test.total)
			ax.set_yscale('linear')
			ax.legend(['Reported positive test cases', 'Reported confirmed cases'])
			ax.set_xlabel('Toatal number of cases')
			ax.set_ylabel('Total number of tests')
			if('yscales' in kwarg.keys()):
				ax.set_yscale(kwarg['yscales'][3])

		elif(kwarg['plot_choice'] == 'Hospitalization'):
			### culmultive hospitalization
			if(sum(df_test['hosp_cum']) > 0):
				ax = fig.add_subplot(gs[0, i])
				y1 = pd.DataFrame(df_test['hosp_cum'][df_test['hosp_cum']>0])
				x = y1.index
				y2 = y1['hosp_cum']/df_test['positive'][df_test['hosp_cum']>0] * 100
				legend1 = ['Hospitalized']
				legend2 = ['%Hospitalized']
				ylabel1 = 'Number of tests'
				ylabel2 = '%Hospitalized'
				ax1, ax2 = plot_tests_bar_and_percent(ax, x, 
													  y1, 
													  y2,
													  ['tab:gray'], 'k', 
													  legend1, legend2, ylabel1, ylabel2)
				myLocator = mticker.MultipleLocator(8)
				ax.xaxis.set_major_locator(myLocator)
				ax.tick_params(axis = 'x', labelrotation = 45)
				ax.set_title(f'{plt_title} Culmultive Hospitalized')
				if('yscales' in kwarg.keys()):
					ax.set_yscale(kwarg['yscales'][4])
				if('resent_scales' in kwarg.keys()):
					if(kwarg['resent_scales'] == True):
						ymax1 = np.max(y1.to_numpy()[-20:,:].flatten())
						ymin1 = np.min(y1.to_numpy()[-20:,:].flatten())
						ymax2 = np.max(y2.to_numpy()[-20:].flatten())
						ymin2 = np.min(y2.to_numpy()[-20:].flatten())
						ax1.set_ylim([min(0, ymin1*1.1), ymax1*1.1])
						ax2.set_ylim([min(0, ymin1*1.1), ymax2*1.1])

			### current hopitalization
			if(sum(df_test['hosp_cur']) > 0):
				ax = fig.add_subplot(gs[1, i])
				y1 = pd.DataFrame(df_test['hosp_cur'][df_test['hosp_cur']>0])
				x = y1.index
				y2 = y1['hosp_cur']/df_test['active'][df_test['hosp_cur']>0] * 100
				legend1 = ['Hospitalized']
				legend2 = ['%Hospitalized']
				ylabel1 = 'Number of Hospitalized Patients'
				ylabel2 = '%Hospitalized'
				ax1, ax2 = plot_tests_bar_and_percent(ax, x, 
													  y1, 
													  y2,
													  ['tab:gray'], 'k', 
													  legend1, legend2, ylabel1, ylabel2)
				myLocator = mticker.MultipleLocator(8)
				ax.xaxis.set_major_locator(myLocator)
				ax.tick_params(axis = 'x', labelrotation = 45)
				ax.set_title(f'{plt_title} Current Hospitalized')
				if('yscales' in kwarg.keys()):
					ax.set_yscale(kwarg['yscales'][5])
				if('resent_scales' in kwarg.keys()):
					if(kwarg['resent_scales'] == True):
						ymax1 = np.max(y1.to_numpy()[-20:,:].flatten())
						ymin1 = np.min(y1.to_numpy()[-20:,:].flatten())
						ymax2 = np.max(y2.to_numpy()[-20:].flatten())
						ymin2 = np.min(y2.to_numpy()[-20:].flatten())
						ax1.set_ylim([min(0, ymin1*1.1), ymax1*1.1])
						ax2.set_ylim([min(0, ymin1*1.1), ymax2*1.1])

			### changes in current hospitalization
			if(sum(df_test['hosp_cur']) > 0):
				ax = fig.add_subplot(gs[2, i])
				y = df_test['hosp_cur'].diff()[df_test['hosp_cur']>0]
				ax.plot(y, color = 'tab:gray')
				myLocator = mticker.MultipleLocator(8)
				ax.xaxis.set_major_locator(myLocator)
				ax.tick_params(axis = 'x', labelrotation = 45)
				ax.set_title(f'{plt_title} Changes in Current Hospitalization')
				if('yscales' in kwarg.keys()):
					ax.set_yscale(kwarg['yscales'][6])
				if('resent_scales' in kwarg.keys()):
					if(kwarg['resent_scales'] == True):
						print('yay')
						ymax = np.max(y.to_numpy()[-20:].flatten())
						ymin = np.min(y.to_numpy()[-20:].flatten())
						ax.set_ylim([min(0, ymin*1.1), ymax*1.1])


def plot_us_tests_by_state(df_us_tests, df_tests_states, usstate_abbs_mapping, **kwarg):
	### parameters
	if('num_states' in kwarg.keys()):
		num_states = kwarg['num_states']
	else:
		num_states = 10
	plot_type = kwarg['plot_type']
	figsize = kwarg['figsize']
	############## figure ############
	fig = plt.figure(figsize = figsize, constrained_layout=True, facecolor="1")

	if(plot_type == 'ranking'):
		gs = fig.add_gridspec(3, 3)
	elif(plot_type == 'relationship'):
		gs = fig.add_gridspec(4, 2)
	elif(plot_type == 'states'):
		gs = fig.add_gridspec(2, 3)

	plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
	plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
	plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
	plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('legend', fontsize=MEDIUM_SIZE)    # legend fontsize
	plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title

	if(plot_type == 'ranking'):
		### value against rank
		states = list(df_us_tests.sort_values(by = 'positive', inplace = False, ascending=False).index[:num_states])
		cat_plots = ['Percent_Pos', 'Test_Per_Million', 'Pos_Per_Million', 'hospitalizedCurrently', 'Perc_Hosp', 'Hosp_Per_Million', 'inIcuCurrently', 'onVentilatorCurrently']
		print('subplot', end = " ")
		for i in range(len(cat_plots)):
			# print(f'{i+1}:{cat_plots[i]}', end = " ")
			print(f'{i+1}', end = " ")
			ax = fig.add_subplot(gs[int(np.floor(i/3)), np.mod(i,3)])

			state_add = list(df_us_tests.sort_values(by = cat_plots[i], inplace = False, ascending=False).index[:2])
			state_list = list(set(states + state_add))
			cat_x = f'rank_{cat_plots[i]}'
			cat_y = cat_plots[i]
			df_plot = df_us_tests.sort_values(by = cat_y, inplace = False, ascending=False)
			x = np.arange(-1, len(df_plot.index)+1)
			p1 = sns.scatterplot(x = cat_x, y = cat_y, 
								 hue = 'Governer_Affiliation', palette = cat_color, 
								 data=df_plot, ax = ax)
			md = np.median(df_plot.loc[df_plot[cat_y]!=0,cat_y])
			ax.plot(x, np.full(len(x), md), '--', color = 'tab:gray')

			for st in state_list:
				pos_st = [df_us_tests.loc[st, cat_x], df_us_tests.loc[st, cat_y]]
				p1.text(pos_st[0]+0.1, pos_st[1] +0.001, st, 
						horizontalalignment='left', size='medium', 
						color='black',weight='normal')
			ax.set_xlim([-0.5, len(df_plot.index)+0.5])
			ax.set_title(f'{cat_y}: US median = {md:.1f}')
	
	elif(plot_type == 'relationship'):
		### relationship between values
		cat_pairs = [['Percent_Pos', 'Test_Per_Million'],
					 ['rank_Percent_Pos', 'rank_Test_Per_Million'],
					 ['Test_Per_Million', 'Pos_Per_Million'],
					 ['rank_Test_Per_Million', 'rank_Pos_Per_Million'], 
					 ['Percent_Pos', 'Pos_Per_Million'],
					 ['rank_Percent_Pos', 'rank_Pos_Per_Million'],
					 ['positive', 'hospitalizedCurrently'],
					 ['Pos_Per_Million', 'Hosp_Per_Million']]
		for i in range(len(cat_pairs)):
			print(f'{i+4}', end = " ")
			# print([int(np.floor(i/2)), np.mod(i,2)])
			ax = fig.add_subplot(gs[int(np.floor(i/2)), np.mod(i,2)])
			p1 = sns.scatterplot(x = cat_pairs[i][0], y = cat_pairs[i][1], ax = ax, data = df_us_tests, 
								 hue = 'Governer_Affiliation', palette = cat_color, legend = False)
			df_fit = pd.DataFrame(columns = ['x', 'y', 'y_pred', 'res'])
			if(i == 1):
				x = np.arange(-1, len(df_us_tests.index) + 1)
				y = max(df_us_tests[cat_pairs[i][0]]) - x
				ax.plot(x, y, '--', color = 'tab:gray')
				df_fit['x'] = df_us_tests[cat_pairs[i][0]]
				df_fit['y'] = df_us_tests[cat_pairs[i][1]]
				df_fit['y_pred'] = max(df_fit['x']) - df_fit['x']
				df_fit['res'] = df_fit['y'] - df_fit['y_pred']
			elif((i == 3) | (i == 5)):
				x = np.arange(-1, len(df_us_tests.index) + 1)
				y = x
				ax.plot(x, y, '--', color = 'tab:gray')
				df_fit['x'] = df_us_tests[cat_pairs[i][0]]
				df_fit['y'] = df_us_tests[cat_pairs[i][1]]
				df_fit['y_pred'] = df_fit['x']
				df_fit['res'] = df_fit['y'] - df_fit['y_pred']
				df_fit['res_abs'] = np.abs(df_fit['res'])
			else:
				df_fit['x'] = df_us_tests[cat_pairs[i][0]]
				df_fit['y'] = df_us_tests[cat_pairs[i][1]]
				z = np.abs(stats.zscore(df_fit[['x', 'y']]))
				df_fit_o = df_fit[(z < 3).all(axis=1)]
				popt = np.polyfit(df_fit_o['x'], df_fit_o['y'], 1)
				df_fit['y_pred'] = popt[0]*df_fit['x'] + popt[1]
				df_fit['res'] = df_fit['y'] - df_fit['y_pred']
				df_fit['res_abs'] = np.abs(df_fit['res'])
				x = np.arange(min(df_fit['x']), max(df_fit['x']), 10)
				y = popt[0]*x + popt[1]
				ax.plot(x, y, '--', color = 'tab:gray')

			states = []
			if(i == 1):
				states += list(df_fit.sort_values(by = 'res').index[:10])
			elif(i == 3):
				states += list(df_fit.sort_values(by = 'res').index[:10])
			else:
				states += list(df_fit.sort_values(by = 'res_abs').index[-10:])

			for st in states:
				pos_st = [df_us_tests.loc[st, cat_pairs[i][0]], df_us_tests.loc[st, cat_pairs[i][1]]]
				p1.text(pos_st[0]+np.random.randint(-2, 2)/2, pos_st[1] +np.random.randint(-2, 2)/2, st, 
						horizontalalignment='left', size='medium', 
						color='black',weight='normal')
	elif(plot_type == 'states'):
		### top 10 states
		print(f'7~9:', end = " ")
		ax1 = fig.add_subplot(gs[0, 0])
		ax2 = fig.add_subplot(gs[0, 1])
		ax3 = fig.add_subplot(gs[0, 2])
		ax4 = fig.add_subplot(gs[1, 0])
		ax5 = fig.add_subplot(gs[1, 1])
		ax6 = fig.add_subplot(gs[1, 2])

		
		max_x3 = 0
		max_y3 = 0
		df_us_tests.sort_values(by = 'positive', ascending = False, inplace = True)
		states_list = list(df_us_tests.index[0:num_states])
		y4 = np.zeros(len(states_list))
		for i, state in enumerate(states_list):
			print(i, end =" ")
			st = state
			df_test = df_tests_states.groupby('state').get_group(st)
			df_test = my_func.reshape_dataframe_v3(df_test)
			# if(i == 0):
			# 	print(df_test.columns)
			x = np.arange(len(df_test.index))
			ax1.plot(df_test.perc_pos, label='_nolegend_')
			ax2.plot(df_test.d_total, label='_nolegend_')
			sns.lineplot(data = df_test, x = 'total', y = 'positive', ax = ax3)
			ax5.plot(df_test.hosp_cur, label='_nolegend_')
			ax6.plot(df_test.hosp_cum)
			y4[i] = df_test.dly_perc_pos[-1]
		x4 = np.arange(len(states_list))
		rects = ax4.bar(x4,y4, label = '_nolegend_', color = 'tab:gray')
		autolabel(rects, ax4, '{:.1f}')

		ax1.tick_params(axis = 'x', labelrotation = 45)
		ax1.set_ylim(top = 55)
		ax1.set_title('Positive%')

		ax2.tick_params(axis = 'x', labelrotation = 45)
		ax2.set_title('Daily Number of Tests')

		ax3.legend(states_list, loc="center right", bbox_to_anchor = (1.01, 0, 0.3, 1))
		ax3.set_title('Positive vs. Total Tests')
		ax3.tick_params(axis = 'x', labelrotation = 45)

		ax4.set_xticks(x4)
		ax4.set_xticklabels(states_list)
		ax4.set_title('Daily Positive%')

		ax5.tick_params(axis = 'x', labelrotation = 45)
		ax5.set_title('Currently Hospitalized')

		ax6.tick_params(axis = 'x', labelrotation = 45)
		ax6.set_yscale('log')
		ax6.set_title('Total Hospitalized')
		ax6.legend(states_list, loc="center right", bbox_to_anchor = (1.01, 0, 0.3, 1))

def county_plot(df_today, states, num_counties, figsize, pct_type):
	def my_autopct_v2(pct):
		return f'{pct*total/100:.0f}({pct:1.1f}%)' if pct > 2 else ''

	############ params ############
	colors = get_colors(num_counties+1, is_last_grey = True)

	############ figure ############
	fig = plt.figure(figsize = figsize, constrained_layout=True, facecolor="1")
	gs = fig.add_gridspec(len(states), 7)

	plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
	plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
	plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
	plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('legend', fontsize=MEDIUM_SIZE)    # legend fontsize
	plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title

	print('States:', end = " ")
	for i, state in enumerate(states):
		print(f'{state},', end = " ")
		df_state = df_today.groupby('Country_Region').get_group('US').groupby('Province_State').get_group(state).loc[:,['Admin2'] + cdra_cols]
		df_state.set_index('Admin2', inplace = True)
		df_state = df_state.loc[(df_state!=0).any(axis=1)]
		df_state.loc[:,'CFR'] = df_state.Deaths/df_state.Confirmed*100
		df_state = df_state.replace([np.inf, -np.inf], 0)

		############ total number pie chart ############
		ax = fig.add_subplot(gs[i,0:2])

		df_state.sort_values(by = 'Confirmed', inplace = True, ascending=False)
		percentages = []
		labels = []
		total = np.sum(df_state.Confirmed)
		for county in list(df_state.index)[:num_counties]:
			percentages.append(df_state.loc[county, 'Confirmed']/np.sum(df_state.Confirmed)*100)
			if(percentages[-1] > 2):
				labels.append(county)
			else:
				labels.append('')
			if(sum(percentages) >= 99):
				break
		if(num_counties < len(df_state.index)):
			labels.append('Rest')
			percentages.append(100-sum(percentages))

		if(pct_type == 1):
			ax.pie(percentages, colors = colors,
				   labels=labels, autopct = my_autopct, 
				   pctdistance=0.8, labeldistance = 1.05,
				   shadow=False, startangle=90,
				   textprops = dict(size = 12))
		elif(pct_type == 2):
			ax.pie(percentages, colors = colors,
				   labels=labels, autopct = my_autopct_v2, 
				   pctdistance=0.7, labeldistance = 1.05,
				   shadow=False, startangle=90,
				   textprops = dict(size = 12))
		ax.axis('equal')
		ax.set_title(f"{state} confirmed cases: {np.sum(df_state.Confirmed):.0f} total")

		############ total deaths pie chart ############
		ax = fig.add_subplot(gs[i,2:4])

		df_state.sort_values(by = 'Deaths', inplace = True, ascending=False)
		percentages = []
		labels = []
		total = np.sum(df_state.Deaths)
		for county in list(df_state.index)[:num_counties]:
			percentages.append(df_state.loc[county, 'Deaths']/np.sum(df_state.Deaths)*100)
			if(percentages[-1] > 2):
				labels.append(county)
			else:
				labels.append('')
			if(sum(percentages) >= 90):
				break
		if(num_counties < len(df_state.index)):
			labels.append('Rest')
			percentages.append(100-sum(percentages))

		if(pct_type == 1):
			ax.pie(percentages, colors = colors,
				   labels=labels, autopct = my_autopct, 
				   pctdistance=0.8, labeldistance = 1.05,
				   shadow=False, startangle=90,
				   textprops = dict(size = 12))
		elif(pct_type == 2):
			ax.pie(percentages, colors = colors,
				   labels=labels, autopct = my_autopct_v2, 
				   pctdistance=0.7, labeldistance = 1.05,
				   shadow=False, startangle=90,
				   textprops = dict(size = 12))
		ax.axis('equal')
		ax.set_title(f"{state} deaths: {np.sum(df_state.Deaths):.0f} total")

		############ cfrs ############
		ax = fig.add_subplot(gs[i,4:])

		df_state.sort_values(by = 'Confirmed', inplace = True, ascending=False)
		y = df_state.CFR[df_state.CFR>0].to_numpy()
		x = np.arange(len(y))
		x1 = np.arange(-2, len(x)+2)
		fr_total = sum(df_state.Deaths)/sum(df_state.Confirmed)*100

		rects = ax.bar(x, y, color = 'grey')
		ax.plot(x1, np.full(len(x1), fr_total), '--', color = 'k')
		a = ax.set_xticks(x)
		a = ax.set_xticklabels(df_state[df_state.CFR>0].index)
		ax.tick_params(axis = 'x', labelrotation = -90)
		autolabel(rects, ax, '{:.2f}')
		ymax = np.ceil(np.max(y) + np.max(y)/10)
		ax.set_ylim([0, ymax])
		ax.set_xlim([-1, len(x) + 0.5])
		a = ax.set_title(f'CFRs: {state} mean = {fr_total:.2f}%')

"""plot different sub-regions of a region."""
### get growth rate by fitting time series data to exponeitial or logistic growth model.
def get_growth_rate(df_ctry, time_str):
	x = np.arange(len(time_str))
	y = df_ctry.Confirmed.to_list()

	res = df_ctry[df_ctry.GF_rolling_thr != 0.0].bfill(axis=1).index[0]
	ind_t0 = max(0, time_str.index(res)-3)
	t = np.arange(len(time_str))[ind_t0:]- ind_t0
	pt = y[ind_t0:]
	
	r = []
	
	try:
		popt_log, pcov_log = opt.curve_fit(my_func.logistic_growth,  t,  pt, maxfev=100000)
		r.append(popt_log[0])
	except RuntimeError:
		r.append(np.inf)
	try:
		popt_log1, pcov_log1 = opt.curve_fit(my_func.logistic_growth,  t,  pt, p0 = (0.1, 100, 1), maxfev=100000)
		r.append(popt_log1[0])
	except RuntimeError:
		r.append(np.inf)
	return np.min(r)


### reformat county ticks
def reformat_xtick(county_list, stat_abbs_mapping):
	new_list = []
	for county in county_list:
		county_strip = [x.strip() for x in county.split(',')]
		# print(county_strip)
		if(len(county_strip) == 2):
			new_list.append(county_strip[0])
		else:
			ct = county_strip[0]
			st = stat_abbs_mapping[county_strip[1]]
			new_list.append(f'{ct}, {st}')
	return new_list

### plots for different sub-regions of a region
def plot_by_regions(df_confirmed, df_deaths, time_datetime, **kwarg):

	############ params ############
	if('num_lineplot' in kwarg.keys()):
		num_lineplot = kwarg['num_lineplot']
	else:
		num_lineplot = 10
	if('num_barplot' in kwarg.keys()):
		num_barplot = kwarg['num_barplot']
	else:
		num_barplot = 20
	if('yscales' in kwarg.keys()):
		yscales = kwarg['yscales']
	else:
		yscales = ['log', 'log', 'log', 'linear', 'log']
	if('k_lines' in kwarg.keys()):
		k_lines = kwarg['k_lines']
	else:
		k_lines = [0.2, 0.4, 0.2, 0.4]
	if('figsize' in kwarg.keys()):
		figsize = kwarg['figsize']
	else:
		figsize = (15, 35)
	if('is_format_xtick' in kwarg.keys()):
		is_format_xtick = kwarg['is_format_xtick']
	else:
		is_format_xtick = False
	if('stat_abbs_mapping' in kwarg.keys()):
		stat_abbs_mapping = kwarg['stat_abbs_mapping']
	if('show_recent' in kwarg.keys()):
		show_recent = kwarg['show_recent']
	else:
		show_recent = False

	color_special = 'k'

	############ figure ############
	fig = plt.figure(figsize = figsize, constrained_layout=True, facecolor="1", dpi = 72)
	# if('suptitle' in kwarg.keys()):
	# 	fig.suptitle(kwarg['suptitle'], fontsize=18, fontweight='bold')

	plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
	plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
	plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
	plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
	plt.rc('legend', fontsize=MEDIUM_SIZE)    # legend fontsize
	plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
	
	print("subplots:", end = " ")

	i_fig = -1
	
	############ Confirmed cases plot ############
	if(kwarg['plot_type'] == 'Confirmed'):
		df_curr = df_confirmed
		thr = 100
	if(kwarg['plot_type'] == 'Deaths'):
		df_curr = df_deaths
		thr = 10
	gs = fig.add_gridspec(4, 3)

	###### total: timepoints ######
	ax = fig.add_subplot(gs[0, 0])
	i_fig += 1
	print(i_fig, end = " ")

	df_curr.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	plot_list = list(df_curr.index[0:num_lineplot])
	if('list_addins' in kwarg.keys()):
		plot_list += kwarg['list_addins']
		plot_list = list(set(plot_list))
		df_plot = df_curr.loc[plot_list,:].sort_values(by = time_datetime[-1], ascending=False)
		plot_list = list(df_plot.index)
	df_plot = df_curr.loc[plot_list,time_datetime]

	colors = get_colors(len(plot_list))
	max_x = 0
	max_y = np.max(df_plot.to_numpy().flatten())

	for i, ele in enumerate(plot_list):
		if(df_plot.loc[ele,time_datetime[-1]]>thr):
			xi = len(df_plot.loc[ele, df_plot.loc[ele,:]>thr].to_numpy())
			if(xi > max_x and ele != 'China'):
				max_x = xi
			if('special' in kwarg.keys()):
				if(ele == kwarg['special']):
					ax.plot(df_plot.loc[ele, df_plot.loc[ele,:]>thr].to_numpy(), linewidth = 3, color = color_special)
				else:
					ax.plot(df_plot.loc[ele, df_plot.loc[ele,:]>thr].to_numpy(), color = colors[i], linewidth = 3)
			else:
				ax.plot(df_plot.loc[ele, df_plot.loc[ele,:]>thr].to_numpy(), color = colors[i])

	x = np.arange(max_x)
	ax.plot(x, thr * (1+k_lines[0]) ** x, ls='--', color='k')
	ax.plot(x, thr * (1+k_lines[1]) ** x, ls='-.', color='k')

	if(is_format_xtick):
		plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

	ax.set_ylim([thr, max_y+max_y/2])
	ax.set_xlim([0, max_x + 2])
	ax.legend(plot_list + 
			   [f'{k_lines[0]*100:.0f}% daily incrase',
			   f'{k_lines[1]*100:.0f}% daily increase'], 
			   loc="center right",
			   bbox_to_anchor = (1.1, 0, 0.25, 1)) #
	ax.set_yscale(yscales[0])
	ax.set_xlabel('Days Since 100 Confirmed Cases')
	ax.set_title(f"{kwarg['plot_type']}")

	###### total confirmed: pie chart ######
	if(kwarg['plot_type'] == 'Confirmed'):
		ax = fig.add_subplot(gs[3,0])
	elif(kwarg['plot_type'] == 'Deaths'):
		ax = fig.add_subplot(gs[2,2])
	i_fig += 1
	print(i_fig, end = " ")

	df_curr.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	percentages = []
	labels = []
	df_curr_total = np.sum(df_curr.loc[:,time_datetime[-1]])

	for ele in df_curr.index[0:num_lineplot]:
		percentages.append(df_curr.loc[ele, time_datetime[-1]]/df_curr_total*100)
		labels.append(ele)

	if(is_format_xtick):
		labels = reformat_xtick(labels, stat_abbs_mapping)

	labels.append('Rest')
	percentages.append(100-sum(percentages))

	colors = get_colors(len(labels), is_last_grey = True)
	wedges, texts, autotexts = ax.pie(percentages, colors = colors, 
									  autopct = my_autopct, pctdistance=0.8,
									  shadow=False, startangle=90,
									  textprops = dict(size = 12))

	ax.legend(wedges, labels, loc="center right", bbox_to_anchor = (1.12, 0, 0, 1))
	ax.axis('equal')
	ax.set_title(f"{kwarg['plot_type']}: Top 10 Pie Chart")

	###### total confirmed: bar_plot ######
	ax = fig.add_subplot(gs[1,0])
	i_fig += 1
	print(i_fig, end = " ")

	df_curr.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	plot_list = list(df_curr.index[0:num_barplot])


	x = np.arange(len(plot_list))
	y = df_curr.loc[plot_list,time_datetime[-1]]
	ax.bar(x, y, color = 'tab:gray')

	if(is_format_xtick):
		plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

	ax.set_xticks(x)
	ax.set_xticklabels(plot_list)
	ax.tick_params(axis = 'x', labelrotation = -90)
	ax.set_title(f"{kwarg['plot_type']}: Top {num_barplot}")
	ax.set_yscale(yscales[3])

	###### total case/million: timepoints ######
	if('Per_Million' in df_curr):
		ax = fig.add_subplot(gs[0, 1])
		i_fig += 1
		print(i_fig, end = " ")
		
		df_curr.sort_values(by = 'Per_Million', inplace = True, ascending=False)
		plot_list = list(df_curr.index[0:num_lineplot])

		if('list_addins' in kwarg.keys()):
			plot_list += kwarg['list_addins']
			plot_list = list(set(plot_list))
			df_plot = df_curr.loc[plot_list,:].sort_values(by = 'Per_Million', ascending=False)
			plot_list = list(df_plot.index)
		df_plot = df_curr.loc[plot_list,time_datetime]

		colors = get_colors(len(plot_list))
		max_x = 0
		max_y = 0
		legend = []

		for i, ele in enumerate(plot_list):
			y = df_curr.loc[ele,time_datetime]/(df_curr.loc[ele,'Population']/MILLION)
			if(y[-1] > thr):
				xi = len(y[y>thr])
				yi = max(y)
				if(xi > max_x and ele != 'China'):
					max_x = xi
				if(yi > max_y):
					max_y = yi
				if('special' in kwarg.keys()):
					if(ele == kwarg['special']):
						ax.plot(y[y>thr].to_numpy(), linewidth = 3, color = color_special)
					else:
						ax.plot(y[y>thr].to_numpy(), color = colors[i], linewidth = 3)
				else:
					ax.plot(y[y>thr].to_numpy(), color = colors[i])
				legend.append(ele)
		if(is_format_xtick):
			legend = reformat_xtick(legend, stat_abbs_mapping)

		x = np.arange(max_x)
		ax.plot(x, thr * (1+k_lines[2]) ** x, ls='--', color='k')
		ax.plot(x, thr * (1+k_lines[3]) ** x, ls='-.', color='k')

		if(max_y > 0):
			ax.set_ylim([thr, max_y+max_y/2])
		if(max_x > 0):
			ax.set_xlim([0, max_x + 2])
		ax.legend(legend + 
				   [f'{k_lines[2]*100:.0f}% daily incrase',
				   f'{k_lines[3]*100:.0f}% daily increase'], 
				   loc="center right",
				   bbox_to_anchor = (1.1, 0, 0.25, 1)) #
		ax.set_yscale(yscales[1])
		ax.set_xlabel('Days Since 100 Confirmed/Million')
		ax.set_title(f"{kwarg['plot_type']}/Million")

	##### total confirmed case/million: bar_plot ######
	if('Per_Million' in df_curr):
		ax = fig.add_subplot(gs[1,1])
		i_fig += 1
		print(i_fig, end = " ")

		df_curr.sort_values(by = 'Per_Million', inplace = True, ascending=False)
		plot_list = list(df_curr.index[0:num_barplot])

		x = np.arange(len(plot_list))
		y = df_curr.loc[plot_list,'Per_Million']
		ax.bar(x, y, color = 'tab:gray')

		if(is_format_xtick):
			plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

		ax.set_xticks(x)
		ax.set_xticklabels(plot_list)
		ax.tick_params(axis = 'x', labelrotation = -90)
		ax.set_title(f"Total Confirmed: Top {num_barplot}")
		ax.set_yscale(yscales[4])
		ax.set_title(f"{kwarg['plot_type']}/Million: Top {num_barplot}")

	##### daily: timepoints ######
	ax = fig.add_subplot(gs[0, 2])
	i_fig += 1
	print(i_fig, end = " ")

	df_curr.sort_values(by = 'New_Today', inplace = True, ascending=False)
	plot_list = list(df_curr.index[0:num_lineplot])
	if('list_addins' in kwarg.keys()):
		plot_list += kwarg['list_addins']
		plot_list = list(set(plot_list))
		df_plot = df_curr.loc[plot_list,:].sort_values(by = 'New_Today', ascending=False)
		plot_list = list(df_plot.index)
	df_plot = df_curr.loc[plot_list,time_datetime]

	colors = get_colors(len(plot_list))
	data_max = 0
	x_max = 0
	for i, ele in enumerate(plot_list):
		if(df_plot.loc[ele,time_datetime[-1]]>thr):
			yi = df_plot.loc[ele,time_datetime].transpose().diff().loc[df_plot.loc[ele,time_datetime]>thr].to_numpy()
			xi = np.arange(len(yi))
			if(show_recent):
				if(np.max(yi[-20:]) > data_max):
					data_max = np.max(yi[-20:])			
			else:
				if(np.max(yi) > data_max):
					data_max = np.max(yi)
			if(ele != 'China' and len(xi) > x_max):
				x_max = len(xi)
			if('special' in kwarg.keys()):
				if(ele == kwarg['special']):
					ax.plot(xi, yi, color = color_special, linewidth = 3)
				else:
					ax.plot(xi, yi, color = colors[i], linewidth = 3)
			else:
				ax.plot(xi, yi, color = colors[i], linewidth = 3)

	if(is_format_xtick):
		plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

	ax.legend(plot_list, loc="center right",
			   bbox_to_anchor = (1.1, 0, 0.25, 1))
	ax.set_yscale(yscales[2])
	ax.set_ylim([0, data_max + data_max/10])
	ax.set_xlim([0, x_max + 2])
	a = ax.set_title(f"Daily {kwarg['plot_type']}")
	ax.set_xlabel('Days Since 100 Confirmed Cases')

	##### daily: bar_plot ######
	ax = fig.add_subplot(gs[1,2])
	
	i_fig += 1
	print(i_fig, end = " ")

	df_curr.sort_values(by = 'New_Today', inplace = True, ascending=False)
	plot_list = list(df_curr.index[0:num_barplot])

	x = np.arange(len(plot_list))
	y = df_curr.loc[plot_list,'New_Today']

	ax.bar(x, y, color = 'tab:gray')

	if(is_format_xtick):
		plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

	ax.set_xticks(x)
	ax.set_xticklabels(plot_list)
	ax.tick_params(axis = 'x', labelrotation = -90)
	ax.set_title(f"Daily {kwarg['plot_type']}: Top {num_barplot}")
	ax.set_yscale(yscales[5])


	##### daily: pie chart ######
	if(kwarg['plot_type'] == 'Confirmed'):
		ax = fig.add_subplot(gs[3,1])
	elif(kwarg['plot_type'] == 'Deaths'):
		ax = fig.add_subplot(gs[3,2])
	i_fig += 1
	print(i_fig, end = " ")

	df_curr.sort_values(by = 'New_Today', inplace = True, ascending=False)
	percentages = []
	labels = []
	df_curr_total = np.sum(df_curr.loc[:,'New_Today'])
	if(df_curr_total > 0):
		for ele in df_curr.index[0:num_lineplot]:
			percentages.append(df_curr.loc[ele, 'New_Today']/df_curr_total*100)
			labels.append(ele)
		# print(percentages)

		if(is_format_xtick):
			labels = reformat_xtick(labels, stat_abbs_mapping)

		labels.append('Rest')
		percentages.append(100-sum(percentages))

		colors = get_colors(len(labels), is_last_grey = True)
		wedges, texts, autotexts = ax.pie(percentages, colors = colors, 
										  autopct = my_autopct, pctdistance=0.8,
										  shadow=False, startangle=90,
										  textprops = dict(size = 12))

	ax.legend(wedges, labels, loc="center right", bbox_to_anchor = (1.12, 0, 0, 1))
	ax.axis('equal')
	ax.set_title(f"Daily {kwarg['plot_type']}: Top 10 Pie Chart")

	##### growth factors: regions with highest growth factors ######
	ax = fig.add_subplot(gs[2, 0])
	i_fig += 1
	print(i_fig, end = " ")


	df_curr.sort_values(by = 'GF_today', inplace = True, ascending=False)
	plot_list = list(df_curr.index[0:int(num_barplot)])
	top_confirmed = list(df_curr.sort_values(by = time_datetime[-1], inplace = False, ascending=False).index[0:int(num_barplot)])

	df_gf = df_curr.loc[plot_list,time_datetime].transpose()
	for ele in plot_list:
		df_ele = pd.DataFrame(df_curr.loc[ele,time_datetime])
		df_ele.loc[:, ele] = df_ele.loc[:, ele].astype('float')
		df_ele = my_func.reshape_dataframe(df_ele, time_datetime)
		df_gf[ele] = df_ele.GF_rolling
	df_gf = df_gf.transpose()
	df_gf.sort_values(by = time_datetime[-1], ascending = False, inplace = True)
	plot_list = list(df_gf.index)
	intersect_list = list(my_func.intersection(plot_list, top_confirmed))
	true_table = [(i in intersect_list) for i in plot_list]

	if(is_format_xtick):
		plot_list = reformat_xtick(plot_list, stat_abbs_mapping)


	x = np.arange(len(plot_list))
	x1 = np.arange(-2, len(x)+2)
	y = df_gf.loc[:, time_datetime[-1]]
	# print(y)

	rects1 = ax.bar(x[true_table], y[true_table], color = 'grey')
	rects2 = ax.bar(x[~np.array(true_table)], y[~np.array(true_table)], color = 'tab:red')

	autolabel(rects1, ax, '{:.1f}')
	autolabel(rects2, ax, '{:.1f}')

	ax.plot(x1, np.full(len(x1), 1), '--', color = 'k')
	a = ax.set_xticks(x)
	a = ax.set_xticklabels(plot_list)
	ax.tick_params(axis = 'x', labelrotation = -90)
	ax.set_xlim([-1, len(x) + 0.5])
	ymax = np.ceil(np.max(y))*1.05
	# print(ymax)
	if(ymax != 0):
		ax.set_ylim(top = ymax)
	ax.set_yscale(yscales[6])
	ax.set_title(f"Growth Factors of {kwarg['plot_type']}: Top growth factors")

	##### growth factors: regions with most cases ######
	ax = fig.add_subplot(gs[2, 1])
	i_fig += 1
	print(i_fig, end = " ")


	df_curr.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	plot_list = list(df_curr.index[0:int(num_barplot)])

	df_gf = df_curr.loc[plot_list,time_datetime].transpose()
	for ele in plot_list:
		df_ele = pd.DataFrame(df_curr.loc[ele,time_datetime])
		df_ele.loc[:, ele] = df_ele.loc[:, ele].astype('float')
		df_ele = my_func.reshape_dataframe(df_ele, time_datetime)
		df_gf[ele] = df_ele.GF_rolling
	df_gf = df_gf.transpose()

	if(is_format_xtick):
		plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

	x = np.arange(len(plot_list))
	x1 = np.arange(-2, len(x)+2)
	y = df_gf.loc[:, time_datetime[-1]]
	# print(y)

	rects1 = ax.bar(x, y, color = 'grey')
	autolabel(rects1, ax, '{:.1f}')
	ax.plot(x1, np.full(len(x1), 1), '--', color = 'k')

	a = ax.set_xticks(x)
	a = ax.set_xticklabels(plot_list)
	ax.tick_params(axis = 'x', labelrotation = -90)
	ax.set_xlim([-1, len(x) + 0.5])
	ymax = np.ceil(np.max(y))*1.05
	# print(ymax)
	if(ymax != 0):
		ax.set_ylim(top = ymax)
	ax.set_yscale(yscales[6])
	ax.set_title(f"Growth Factors of {kwarg['plot_type']}: Top cases")
		
	if(kwarg['plot_type'] == 'Deaths'):
		##### fatality rate: most caes ######
		ax = fig.add_subplot(gs[3, 0])
		i_fig += 1
		print(i_fig, end = " ")

		df_confirmed.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
		plot_list = list(df_confirmed.index[0:num_barplot])
		# plot_list = [ele for ele in df_confirmed.index if df_confirmed.loc[ele,time_datetime[-1]] >= 500]
		fatal_rates = df_deaths.loc[plot_list,time_datetime] / df_confirmed.loc[plot_list,time_datetime] * 100
		x = np.arange(len(plot_list))
		x1 = np.arange(-2, len(x)+2)
		fr_total = np.sum(df_deaths.loc[:,time_datetime[-1]]) / np.sum(df_confirmed.loc[:,time_datetime[-1]]) * 100
		y = fatal_rates.loc[plot_list,time_datetime[-1]]
		# print(y)

		if(is_format_xtick):
			plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

		rects1 = ax.bar(x[y<=fr_total], y[y<=fr_total], color = 'tab:grey')
		rects2 = ax.bar(x[y>fr_total], y[y>fr_total], color = 'tab:red')
		ax.plot(x1, np.full(len(x1), fr_total), '--', color = 'k')

		autolabel(rects1, ax, '{:.1f}')
		autolabel(rects2, ax, '{:.1f}')

		a = ax.set_xticks(x)
		a = ax.set_xticklabels(plot_list)
		ax.tick_params(axis = 'x', labelrotation = -90)
		ax.set_xlim([-1, len(x) + 0.5])

		ymax = np.ceil(np.max(y))*1.1
		# print(ymax)
		if(ymax != 0):
			ax.set_ylim([0, ymax])
		# ax.set_yscale(yscales[7])

		a = ax.set_title(f'CFR of regions with most cases (mean = {fr_total:.2f}%)')


		########### fatality rate: highest CFR ############
		ax = fig.add_subplot(gs[3, 1])
		i_fig += 1
		print(i_fig, end = " ")

		fatal_rates = df_deaths.loc[:,time_datetime] / df_confirmed.loc[:,time_datetime] * 100
		fatal_rates[~np.isfinite(fatal_rates)] = 0
		fatal_rates.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
		plot_list_ori = list(fatal_rates.index[0:num_barplot])
		plot_list = []
		for rg in plot_list_ori:
			if('Unassigned' not in rg):
				plot_list.append(rg)
		x = np.arange(len(plot_list))
		x1 = np.arange(-2, len(x)+2)
		fr_total = np.sum(df_deaths.loc[:,time_datetime[-1]]) / np.sum(df_confirmed.loc[:,time_datetime[-1]]) * 100
		y = fatal_rates.loc[plot_list,time_datetime[-1]]
		# print(y)

		if(is_format_xtick):
			plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

		rects1 = ax.bar(x, y, color = 'tab:grey')
		ax.plot(x1, np.full(len(x1), fr_total), '--', color = 'k')

		autolabel(rects1, ax, '{:.1f}')

		a = ax.set_xticks(x)
		a = ax.set_xticklabels(plot_list)
		ax.tick_params(axis = 'x', labelrotation = -90)
		ax.set_xlim([-1, len(x) + 0.5])

		ymax = np.ceil(np.max(y))*1.1
		# print(ymax)
		if(ymax != 0):
			ax.set_ylim([0, ymax])
		# ax.set_yscale(yscales[7])

		a = ax.set_title(f'Regions of highest CFRs (mean = {fr_total:.2f}%)')

	

	########## r for logistic growth fit ############
	# ax = fig.add_subplot(gs[4, ncol_half:])
	# print("9:", end = " ")
	
	# df_confirmed.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	# plot_list = list(df_confirmed.index[0:num_barplot])

	# x = np.arange(len(plot_list))
	# x1 = np.arange(-2, len(x)+2)
	# # if not 'rs_global' in locals():
	# rs_global = []
	# for i in x:
	# 	print(f'{i}-', end = "")
	# 	ele = plot_list[i]
	# 	df_ele = pd.DataFrame(df_confirmed.loc[ele,time_datetime])
	# 	df_ele.loc[:, ele] = df_ele.loc[:, ele].astype('float')
	# 	df_ele = my_func.reshape_dataframe(df_ele, time_datetime)
	# 	rs_global.append(get_growth_rate(df_ele, time_datetime))
	# rs_global = np.array(rs_global)

	# rects = ax.bar(x[rs_global<1], rs_global[rs_global<1], color = 'tab:grey')
	# ax.bar(x[rs_global>=1], rs_global[rs_global>=1], color = 'tab:grey')
	# ax.plot(x1, np.full(len(x1), np.median(rs_global)), '--', color = 'k')

	# autolabel(rects, ax, '{:.2f}')

	# if(is_format_xtick):
	# 	plot_list = reformat_xtick(plot_list, stat_abbs_mapping)

	# a = ax.set_xticks(x)
	# a = ax.set_xticklabels(plot_list)
	# ax.tick_params(axis = 'x', labelrotation = -90)
	# ax.set_xlim([-1, len(x) + 0.5])

	# ax.set_ylim([0, 1])
	# ax.set_yscale(yscales[3])
	
	# a = ax.set_title(f'Logistic fitted r, median = {np.median(rs_global):.2f}')
	

	# plt.tight_layout()

### plot percentage change for top sub-regions of a given region over time.
def plot_percentage_over_time(df, n, **kwarg):
		
	time_datetime = list(df.columns)

	if('title' in kwarg.keys()):
		title = kwarg['title']
	else:
		title = 'Percentage of regions over time'

	def format_fn(tick_val, tick_pos):
		if(tick_val >= 0 and tick_val < len(time_datetime)):
			return time_datetime[int(tick_val)].date()
		else:
			return None
		
	labels = []
	percentages = np.zeros((len(time_datetime), n+1))
	total_labels = []
	label_lists = []
	for x in range(len(time_datetime)):
		time = time_datetime[x]
		# print(time)
		# print(df)
		df.sort_values(by = time, inplace = True, ascending=False)
		df_time_total = np.sum(df.loc[:,time])
		ll = []
		for i, state in enumerate(df.index[0:n]):
			if(df.loc[state, time] > 0):
				ll.append(state)
				label_lists.append(state)
			else:
				ll.append('')
			percentages[x,i] = (df.loc[state, time]/df_time_total*100)
		percentages[x,n] = (100-np.sum(percentages[x,:]))
		ll.append('Others')
		total_labels.append(ll)
	label_lists = list(set(label_lists))
	
	colors_bar = get_colors(len(label_lists))
	color_codes = {}
	for i, label in enumerate(label_lists):
		color_codes[label] = colors_bar[i]
	label_lists.append('Others')
	color_codes['Others'] = [0.85, 0.85, 0.85, 1]
	
	is_label = {}
	for label in label_lists:
		is_label[label] = 1
	fix, ax = plt.subplots(1,1, figsize = (15,5))
	legend = []
	
	for x in range(len(time_datetime)):
		print(f'{x}-', end = "")
		state = total_labels[x][0]
		if(is_label[state] == 1):
			plt.bar(x, percentages[x, 0], color = color_codes[state])
			legend.append(state)
			is_label[state] = 0
		else:
			plt.bar(x, percentages[x, 0], color = color_codes[state], label='_nolegend_')
		for i, percent in enumerate(percentages[x, 1:]):
			state = total_labels[x][i+1]
			if(state):
				if(is_label[state] == 1):
					plt.bar(x, percent, bottom = np.sum(percentages[x, 0:i+1]), color = color_codes[state])
					legend.append(state)
					is_label[state] = 0
				else:
					plt.bar(x, percent, bottom = np.sum(percentages[x, 0:i+1]), color = color_codes[state], label='_nolegend_')
	
	ax.xaxis.set_major_formatter(mticker.FuncFormatter(format_fn))
	myLocator = mticker.MultipleLocator(4)
	ax.xaxis.set_major_locator(myLocator)
	ax.tick_params(axis = 'x', labelrotation = 45)
	ax.set_xlim([-1, len(time_datetime) + 0.5])
	ax.set_ylabel('Percentage')
	plt.title(title)
	plt.legend(legend, loc='upper center', bbox_to_anchor=(0.5, -0.25), ncol= int(len(legend)/3))

"""world case vs. population"""
def get_residuals(df_fit, fit_intercept, cat_x, cat_y):
	X = np.array([df_fit[cat_x]]).transpose()
	y = df_fit[cat_y].to_numpy()
	reg = LinearRegression(fit_intercept = fit_intercept).fit(X, y)
	y_predicted = reg.predict(X)
	residuals = (y-y_predicted)
	df_fit.loc[:,'Residuals'] = residuals
	return df_fit, reg

def add_country_annotations(ax, p1, df_fit, countries, color, cat_x, cat_y):
	for ctry in countries:
		pos = [df_fit.loc[ctry, cat_x], 
				  df_fit.loc[ctry, cat_y]]
		ax.plot(pos[0], pos[1], 'o', color = color)
		p1.text(pos[0]+0.01, pos[1] +0.01, ctry, 
				horizontalalignment='left', size='medium', 
				color='black',weight='normal')
	return ax

def get_annotation_list(df_fit, key_countries, is_joint):
	countries = []
	if(is_joint):
		countries += list(df_fit.sort_values(by = 'Residuals').index[-3:])
		countries += list(df_fit.sort_values(by = 'Residuals').index[:3])
		countries += my_func.intersection(list(df_fit.index), key_countries)
		countries = list(set(countries))
	else:
		countries += list(df_fit.sort_values(by = 'Residuals').index[-5:])
		countries += list(df_fit.sort_values(by = 'Residuals').index[:5])
	return countries


def world_cases_vs_population(key_countries, df_ctry_today):
	
	fig = plt.figure(figsize = (15, 10), constrained_layout=True, facecolor="1")
	gs = fig.add_gridspec(2,3)

	###
	ax = fig.add_subplot(gs[0,0])
	print("1-", end = "")
	cat_x = 'Total_Confirmed'
	cat_y = 'New_Confirmed'
	df_fit = df_ctry_today[[cat_x, cat_y]]
	df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
	p1 = sns.regplot(data = df_fit, 
					 x = cat_x, y = cat_y, ax = ax, scatter_kws={'alpha':0.3})
	df_fit, reg = get_residuals(df_fit, False, cat_x, cat_y)
	countries = get_annotation_list(df_fit, key_countries, True)
	ax = add_country_annotations(ax, p1, df_fit, countries, 'tab:blue', cat_x, cat_y)
	ax.set_xscale('log')
	ax.set_yscale('log')
	# ax.set_title(f'All countries: \n {cat_x} vs {cat_y} \n y={10**reg.intercept_:.2f}*x^{reg.coef_[0]:.1f}' )
	ax.set_title(f'All countries: \n {cat_x} vs {cat_y} \n y={reg.coef_[0]:.2f}*x')

	###
	ax = fig.add_subplot(gs[1,0])
	print("2-", end = "")
	cat_x = 'Total_Confirmed'
	cat_y = 'Total_Deaths'

	df_fit = df_ctry_today.loc[df_ctry_today.Total_Confirmed>100, [cat_x, cat_y]]
	df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
	p1 = sns.regplot(data = df_fit, 
					 x = cat_x, y = cat_y, ax = ax, scatter_kws={'alpha':0.3})
	df_fit, reg = get_residuals(df_fit, True, cat_x, cat_y)
	countries = get_annotation_list(df_fit, key_countries, True)
	ax = add_country_annotations(ax, p1, df_fit, countries, 'tab:blue', cat_x, cat_y)
	ax.set_xscale('log')
	ax.set_yscale('log')
	ax.set_title(f'Countries w/ confirmed cases >100: \n {cat_x} vs {cat_y}\n y={reg.coef_[0]:.2f}*x')

	###
	cat_x = 'Population'
	cat_y = 'Pos_per_Million'
	cat = 'Confirmed_Cat'

	df_fit = np.log10(df_ctry_today[[cat_x, cat_y]])
	df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
	df_fit[cat] = df_ctry_today[cat]
	cats = list(set(df_ctry_today[cat]))

	for i in [0,1]:
		print(f"{i}-", end = "")
		ax = fig.add_subplot(gs[i, 1])
		if(i == 0):
			df_fit = np.log10(df_ctry_today[[cat_x, cat_y]])
			df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
			df_fit[cat] = df_ctry_today[cat]
			for icat in cats:
				p1 = sns.regplot(data = df_fit[df_fit[cat] == icat], 
							x = cat_x, y = cat_y, ax = ax)
			ax.legend(cats, title = cat)
			ax.set_title(f'All countries:\n{cat_x} vs {cat_y}')
		if(i == 1):
			icat = [i for i in cats if 'above' in i][0]
			df_fit = np.log10(df_ctry_today.groupby(cat).get_group(icat)[[cat_x, cat_y]])
			df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
			if(cats.index(icat) == 0):
				color = 'tab:blue'
			else:
				color = 'tab:orange'
			p1 = sns.regplot(data = df_fit, 
							x = cat_x, y = cat_y, ax = ax, color = color, scatter_kws={'alpha':0.3})
			# ax.set_title(f'Confirmed cases {icat}:\n{cat_x} vs {cat_y}\n y={10**reg.intercept_:.2f}*x^{reg.coef_[0]:.1f}')
			ax.set_title(f'Confirmed cases {icat}:\n{cat_x} vs {cat_y}')
			df_fit, reg = get_residuals(df_fit, True, cat_x, cat_y)
			# ax = add_country_annotations(ax, p1, df_fit, key_countries, color, cat_x, cat_y)


	##
	cat_x = 'Total_Confirmed'
	cat_y = 'Pos_per_Million'
	cat = 'Population_Cat'

	df_fit = np.log10(df_ctry_today[[cat_x, cat_y]])
	df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
	df_fit[cat] = df_ctry_today[cat]
	cats = list(set(df_ctry_today[cat]))

	for i in [0,1]:
		print(f"{i}-", end = "")
		ax = fig.add_subplot(gs[i, 2])
		if(i == 0):
			df_fit = np.log10(df_ctry_today[[cat_x, cat_y]])
			df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")
			df_fit[cat] = df_ctry_today[cat]
			for icat in cats:
				p1 = sns.regplot(data = df_fit[df_fit[cat] == icat], 
							x = cat_x, y = cat_y, ax = ax)
			ax.legend(cats, title = cat)
			ax.set_title(f'All countries:\n{cat_x} vs {cat_y}')
		if(i == 1):
			ax = fig.add_subplot(gs[i,2])
			icat = [i for i in cats if 'above' in i][0]
			df_fit = np.log10(df_ctry_today.groupby(cat).get_group(icat)[[cat_x, cat_y]])
			df_fit = df_fit.replace([np.inf, -np.inf], np.nan).dropna(how="any")

			if(cats.index(icat) == 0):
				color = 'tab:blue'
			else:
				color = 'tab:orange'

			p1 = sns.regplot(data = df_fit, 
							x = cat_x, y = cat_y, ax = ax, color = color, scatter_kws={'alpha':0.3})
			# ax.set_title(f'Population {icat}:\n{cat_x} vs {cat_y}\n y={10**reg.intercept_:.2f}*x^{reg.coef_[0]:.1f}')
			ax.set_title(f'Population {icat}:\n{cat_x} vs {cat_y}')
			df_fit, reg = get_residuals(df_fit, True, cat_x, cat_y)
			countries = get_annotation_list(df_fit, key_countries, True)
			# ax = add_country_annotations(ax, p1, df_fit, countries, color, cat_x, cat_y)

"""China"""
def plot_china_prov(df_mc_confirmed, df_mc_deaths, df_hb, df_co, **kwarg):
	######### unravel params
	if('is_fitting' in kwarg.keys()):
		is_fitting = kwarg['is_fitting']
		if('fitting_params' in kwarg.keys()):
			fitting_params = kwarg['fitting_params']
		else:
			fitting_params = {}
	else:
		is_fitting = False
	if('plotting_params' in kwarg.keys()):
		plotting_params = kwarg['plotting_params']
	else:
		cat_color = {'Confirmed':'tab:blue',
					'Deaths':'tab:orange', 
					'Recovered':'tab:green', 
					'Active':'tab:red'}
		plotting_params = {
			'figsize':(15, 15),
			'time_series_cols' : ['Confirmed', 'Deaths', 'Recovered', 'Active'],
			'locator_param' : 4,
			'num_of_rols' : 6,
			'cat_color' : cat_color
		}
	time_datetime = list(df_hb.index)

	fig = plt.figure(figsize = plotting_params['figsize'], constrained_layout=True, facecolor="1")
	gs = fig.add_gridspec(plotting_params['num_of_rols'],2)
	
	############ China confirmed pie chart
	ax = fig.add_subplot(gs[0,0])

	df_mc_confirmed.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	percentages = []
	labels = []
	df_mc_confirmed_total = np.sum(df_mc_confirmed.loc[:,time_datetime[-1]])
	for iprov in df_mc_confirmed.index[0:10]:
		percentages.append(df_mc_confirmed.loc[iprov, time_datetime[-1]]/df_mc_confirmed_total*100)
		labels.append(iprov)
	labels.append('Rest')
	percentages.append(100-sum(percentages))

	colors = get_colors(len(labels), is_last_grey = True)
	wedges, texts, autotexts = ax.pie(percentages, colors = colors, 
									  autopct = my_autopct, pctdistance=0.8,
									  shadow=False, startangle=90,
									  textprops = dict(size = 12))
	ax.legend(wedges, labels, loc="center right", bbox_to_anchor = (1, 0, 0, 1))
	ax.axis('equal')
	ax.set_title("Total Confirmed Cases in China")

	############ China deaths pie chart
	ax = fig.add_subplot(gs[0,1])

	df_mc_deaths.sort_values(by = time_datetime[-1], inplace = True, ascending=False)
	percentages = []
	labels = []
	df_mc_deaths_total = np.sum(df_mc_deaths.loc[:,time_datetime[-1]])
	for iprov in df_mc_deaths.index[0:10]:
		percentages.append(df_mc_deaths.loc[iprov, time_datetime[-1]]/df_mc_deaths_total*100)
		labels.append(iprov)
	labels.append('Rest')
	percentages.append(100-sum(percentages))

	colors = get_colors(len(labels), is_last_grey = True)
	wedges, texts, autotexts = ax.pie(percentages, colors = colors, 
									  autopct = my_autopct, pctdistance=0.8,
									  shadow=False, startangle=90,
									  textprops = dict(size = 12))
	ax.legend(wedges, labels, loc="center right", bbox_to_anchor = (1, 0, 0, 1))
	ax.axis('equal')
	ax.set_title("Total Deaths in China")

	x = np.arange(len(time_datetime))
	y1 = df_hb.Confirmed.to_list()
	y2 = df_co.Confirmed.to_list()
	y3 = df_hb.Deaths.to_list()
	y4 = df_co.Deaths.to_list()

	ind_t0 = len(time_datetime)
	t = np.arange(len(time_datetime))[:ind_t0]
	pt1 = y1[:ind_t0]
	pt2 = y2[:ind_t0]

	# popt_exp, pcov_exp = opt.curve_fit(exp_growth,  t,  p_t)
	popt1, pcov1 = opt.curve_fit(my_func.logistic_growth,  t,  pt1, maxfev=10000)
	# popt1, pcov1 = opt.curve_fit(my_func.logistic_growth,  t,  pt, p0 = [0.1, 100, 1], maxfev=10000)
	popt2, pcov2 = opt.curve_fit(my_func.logistic_growth,  t,  pt2, maxfev=10000)
	popt3, pcov3 = opt.curve_fit(my_func.logistic_growth,  x,  y3, maxfev=10000)
	popt4, pcov4 = opt.curve_fit(my_func.logistic_growth,  x,  y4, maxfev=10000)
	
	future = 0
	x1 = np.arange(len(x) + future)
	xt1 = time_datetime
	xt2 = my_func.get_datetime_arange(time_datetime[0], len(x1))

	ax1 = fig.add_subplot(gs[1, 0])
	plt.plot(xt1, y1, '.', ms = 10, color = plotting_params['cat_color']['Confirmed'])
	plt.plot(xt2, my_func.logistic_growth(x1, popt1[0], popt1[1], popt1[2]), '-', color = plotting_params['cat_color']['Confirmed'])
	c1_end = my_func.logistic_growth(x1, popt1[0], popt1[1], popt1[2])[-1]
	plt.plot(xt1, y3, '*', ms = 10, color = plotting_params['cat_color']['Deaths'])
	plt.plot(xt2, my_func.logistic_growth(x1, popt3[0], popt3[1], popt3[2]), '-', color = plotting_params['cat_color']['Deaths'])
	d1_end = my_func.logistic_growth(x1, popt3[0], popt3[1], popt3[2])[-1]

	plt.title(f'Logistic Growth Fit for Hubei, r = {popt1[0]:.2f}')
	myLocator = mticker.MultipleLocator(4)
	ax1.xaxis.set_major_locator(myLocator)
	ax1.tick_params(axis = 'x', labelrotation = 45)
	# _ = ax1.set_xlim(right = len(time_datetime))
	ax1.legend(['Confirmed', 
				f'Confirmed Logistic Fit: {c1_end:.0f}', 
				'Deaths', 
				f'Deaths Logistic Fit: {d1_end:.0f}'])


	ax2 = fig.add_subplot(gs[1, 1])
	plt.plot(x, y2, '.', ms = 10, color = plotting_params['cat_color']['Confirmed'])
	plt.plot(x1, my_func.logistic_growth(x1, *popt2), '-', color = plotting_params['cat_color']['Confirmed'])
	c2_end = my_func.logistic_growth(x1, *popt2)[-1]
	plt.plot(x, y4, '*', ms = 10, color = plotting_params['cat_color']['Deaths'])
	plt.plot(x1, my_func.logistic_growth(x1, *popt4), '-', color = plotting_params['cat_color']['Deaths'])
	d2_end = my_func.logistic_growth(x1, *popt4)[-1]

	plt.title(f'Logistic Growth Fit for Other Chinese Provs, r = {popt2[0]:.2f}')
	myLocator = mticker.MultipleLocator(4)
	ax2.xaxis.set_major_locator(myLocator)
	ax2.tick_params(axis = 'x', labelrotation = 45)
	ax2.legend(['Confirmed', 
				f'Confirmed Logistic Fit: {c2_end:.0f}', 
				'Deaths', 
				f'Deaths Logistic Fit: {d2_end:.0f}'])
