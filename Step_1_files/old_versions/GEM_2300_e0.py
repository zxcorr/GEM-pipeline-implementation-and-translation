'''
'''
import numpy as np
import math
import time
import heapq as hp

print("\n \n")
print("------------------------------------ GEM 0 : CONVERSION OF DIGITAL TO PHYSICAL UNITS -------------------------------------------")
print("\n")

def main():
	
	jlist = int(input("Single file (1) or Multiple files (2)?: \n"))
	print("\n")

#::::::::::::::::::::: encoders section :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

	V_180   = 4.974     # effective half scale for elevation - Cachoeira
	Vmaxel  = 9.995     # maximum voltage reading of elevation encoder
	jset = int(input("Set Vmax? yes -> 1, no -> 2: \n")) #for the azimuth encoder
	print("\n")
	
	if jset == 1:
		Vmaxaz = float(input("Write Vmax value: "))
	else:
		Vmaxaz = 9.8993 #Cachoeira
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::	
	azoff   =   0.00                # Uncalibrated encoder    
	encoff  = 360.0*(1.0-1.0/4096)  # angle of encoder step at V_max
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	
	if jlist == 1:
		Dir0 = ""
		fullname = input("ENTER the .txt file name: \n")
	else:
		file_list = input("ENTER the file list name: \n")
	print("\n")
	
	#jval = int(input("Evaluate Temperature statistics alone? yes -> 1, no -> 2: \n"))
	
	if jlist == 2:
		Dir = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/txt/" # filelist dir   
		Dir1 = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/time_check/" # dat/time_check
		Dir2 = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/time_offset/" # dat/time_offset
		Dir3 = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/log_/" # dat/log_
		Dir4 = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/Tem/Temps/" # dat/Tem/Temps
		Dir5 = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/txt/txt/" # input dir (filename)
		Dir6 = "/media/nicolli/eabdalla2012/resultados_nicolli/tradução_gem_modulos/Step_1_files/dat/" # dat dir
		
		with open(Dir + file_list) as arq:
			filelist = arq.readlines()
			n_files = len(filelist)
			prevs_jffin = [0]
			jfini_list = []
			jffin_list = []
			log_data = []
			for file_name_index in range(n_files):
				file_name = filelist[file_name_index]
				file_name = file_name.replace("\t", "")
				file_name = file_name.replace("\n", "")
				log_data = np.genfromtxt(Dir5 + file_name, dtype = float, usecols = (0))
				count = len(log_data)  
				jfini = log_data[0]
				jffin = log_data[-1]
				jffin_list.append(jffin)
				jfini_list.append(jfini)
				prevs_jffin.append(jffin)
				diff = jffin - jfini + 1
				
				data1 = str(file_name_index)
				for item in [jfini, jffin, diff, count]:
					data1 += f"\t{item}"
				flist_name = file_list.replace(".txt", "_log.dat")
				
				if file_name_index == 0:                           
					with open(Dir3 + flist_name, "w") as arq1:
						arq1.write("file\tinit\tlast\tdiff\tcount\n")
						arq1.write(data1)
						arq1.write("\n")
				else:
					with open(Dir3 + flist_name, "a") as arq1:
						arq1.write(data1)
		newlist = str(input("ENTER the filelist name of the converted .dat files: \n"))
			 	  	
#::::::::::::::::::::: Start Conversion :2::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	mit = 0
	mdtime = 0
	mdtaux = 0
	tioff = 0
	description_check = "\tfile_name\tfiletime\tfilespan\tframetime\tframespan\tN_frames\tdtime\n"
	if jlist == 2:
	#:::::::::: Iteration_loop ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
		while(abs(mdtime) < abs(mdtaux) or mit <= 1): 
			mit += 1 
			with open(Dir + file_list) as arq:
				filelist = arq.readlines()
				n_files = len(filelist)
				for file_name_index in range(n_files):
					file_name = filelist[file_name_index]
					file_name = file_name.replace("\t", "")
					file_name = file_name.replace("\n", "")
					save_name = file_name[:8]	
					dtime = []
					with open(Dir5 + file_name) as arq3:
				#::::::::::::::: Files_loop :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
						if file_name_index == 0:
							z = 0
						else:
							z += 1
						if diff == count or jlist == 1: 
							jyear = 1900+int(file_name[0:2])
							jmonth = int(file_name[3:5])
							jday = int(file_name[6:8]) 	
							jjfilenum = file_name[8:16]
							jfilenum = int(file_name[8:16])
							jhh = int(jjfilenum[0:2])
							jmm = int(jjfilenum[2:4])
							jss = int(jjfilenum[4:6])
							ml = (int(jjfilenum[6:]))/100
							sms = ""			
							if ml >= 0.50:
								jss = jss -1
								jjss = str(jss)
						
								mll = str(int(ml * 100))
								ms = jjss + mll 
								sms = f"{ms}"
							fullname = file_name[0:12]+sms 
							#tempo inicial de observação =>
							timeini = jhh*3600 + jmm*60 + jss + ml
							if jlist == 2:
								data2 = str(file_name[0:8])
								s_jfilenum = str(jfilenum)
								txt = ".dat"
								underline = "_"
								s_data = data2 + s_jfilenum + txt
								#for item in [jfilenum, jday, jmonth, jyear]:
									#data2 += f"{item}\t"
								if file_name_index == 0:
									with open(Dir + newlist, "w") as arq2:
										arq2.write(s_data)
										arq2.write("\n")
								else:
									with open(Dir + newlist, "a") as arq2:
										arq2.write(s_data)
										arq2.write("\n")	 
								
							j99a = 0
							j99b = 1
							tag = file_name[0:16]
							previous_timeini = timeini #######################################
							timeaux = previous_timeini	 	
						else:
							j99a = 99
							j99b = jffin_list[file_name_index]-(count-100)-(prevs_jffin[z]) 
							timefin = 0
							timeini = timefin + j99b*0.56002 
							jhh = int(timeini/3600)
							jmm = int((timeini - jhh*3600)/60)
							ms = (timeini - jhh*3600 - jmm*60)*100
							previous_timeini = timeini
							timeaux = previous_timeini
							print("timeaux", timeaux)
							if (timeini - timeaux) < 0.0:
								jhh = jhh + 24
							if jhh >= 24:
								jhh = jhh - 24
								jday = jday +1
								if jday > 31:
									jmonth += 1
									if jmonth > 12:
										jmonth = jmonth - 12
										jyear += 1
							jjmonth = str(jmonth)
							jjday = str(jday)
							jjhh = str(jhh)
							jjmm = str(jmm)
							sms = str(ms)			
							jfilenum = jhh*10**6 + jmm*10**4 + ms		
							month = f"{jjmonth:02}"
							day = f"{jjday:02}"
							hh = f"{jjhh:02}"
							mm = f"{jjmm:02}"
							ssms = f"{sms:04}"
							filenum = hh + mm + ssms
							jfilename = file_name[0:8]+filenum
							#print("jfilename", jfilename)
							data3 = str(int(file_name[0:8]))
							for item in [jfilenum, jday, jmonth, jyear]:
								item = f"{item:.3f}"
								data3 += f"\t{item}"
							with open(Dir+jfilename, w) as arq4:
								arq4.write(data3)
								arq4.write("\n")
							tag = file_name[0:8]+jfilenum

						if (file_name_index==0) or (jfini_list[file_name_index]-prevs_jffin[z] != 1):
							lm = 1
							timefin = timeini - 0.56002
							timefra = (jffin_list[file_name_index]-jfini_list[file_name_index]+1)*0.56002
							timediff = 0.0        
							timeini = timeini + tioff
							mdtaux	= mdtime
							mdtime = tioff
							dtime.append(tioff) # GPS synchronized files should have dtime = 0
							data4 = str(file_name_index)
							data4 += "\t" + tag
							for item in [timeini, timediff, timefin + 0.56002, timefra,\
							int(timefra/0.56002), dtime[-1]]:###############################################
								item = f"{item:.2f}"
								data4 += f"\t{item}\t"
							name_check = file_list.replace(".txt", "_check.dat")
							if file_name_index == 0:
								with open(Dir1 + name_check, "w") as arq5:
									arq5.write(description_check)
									arq5.write(data4)
									arq5.write("\n") 
							else:
								with open(Dir1 + name_check, "a") as arq5:
									arq5.write(data4)
									arq5.write("\n") 
						else:
							lm = 0
							timediff = timeini - timeaux
							if timediff < 0.0:
								timediff += 86400.0 
								
							timefin = float(timeini - 0.56002)	
							frametime = float(timefin + j99b*0.56002) #time of 1st frame in file
							jframespan = jffin_list[file_name_index] - jfini_list[file_name_index] + j99a + 1	#difference between 1st frames
							timefra = jframespan*0.56002	
							
							if float(timeini - frametime) < 0.56002:
								lm += 1
								previous_lm = lm
								dtime.append(float(timeini-frametime))
								mdtime	= mdtime + dtime[-1]
								
							data5 = str(file_name_index)
							data5 += "\t"+tag
							for item in [timeini, timediff, frametime,timefra, jframespan, dtime[-1]]:
								item = f"{item:.2f}"
								data5 += f"\t{item}\t"
							name_check = file_list.replace(".txt", "_check.dat")
							if file_name_index == 0:
								with open(Dir1 + name_check, "w") as arq6: #####
									arq6.write(description_check)
									arq6.write(data5)
									arq6.write("\n")
							else:
								with open(Dir1 + name_check, "a") as arq6: #####
									arq6.write(data5)
									arq6.write("\n")
							
					#:::::::: Data conversion :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
						with open(Dir5 + file_name, "r+") as f:
							input_data = f.readlines()
							n_lines = len(input_data)
							jframe, jelevat, jsignal, jt1, jt2, jt3, jt4, jtns, jvns, jhi, jazimut = \
							[], [], [], [], [], [], [], [], [], [], []
						
							for line in input_data:
								input_line = line.replace(" ", "")
								input_line = line.replace("\n", "")
								input_line = input_line.split(" ")
								input_line = list(filter(None, input_line))
								jframe.append(input_line[0])
								jelevat.append(input_line[1])
								jsignal.append(input_line[2])
								jt1.append(input_line[4])
								jt2.append(input_line[5])
								jt3.append(input_line[6])
								jt4.append(input_line[7])
								jhi.append(input_line[9])	
								jtns.append(input_line[11])
								jvns.append(input_line[12])
								jazimut.append(input_line[13])
						
					#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
						out_data = [] # output data array
						out_line = []
						elt, azi, sig, lt1, lt2, lt3, lt4, ltns, lvns, lhi, time_t = [], [], [],\
						[], [], [], [], [], [], [], [] 
						i_frames = len(jframe)
						k = 0
						for i in range(i_frames):
							k += 1
							#::::::::: Pointing direction ::::::::::::::::::::::::::::::::::::::::::::::::::
							#in degrees 					
							elevat  = (conversion_fun(jelevat[i]) - V_180)/Vmaxel*encoff 
							azimut  = conversion_fun(jazimut[i])/Vmaxaz*encoff + azoff	
							elt.append(elevat)
							azi.append(azimut)
							
							#::::::::: Radiometer signal :::::::::::::::::::::::::::::::::::::::::::::::::::
							# in volts
							signal  = conversion_fun(jsignal[i])
							sig.append(signal)

							#::::::::: Temperature sensors readouts in Celsius :::::::::::::::::::::::::::::
							"""HEMT amplifier"""		                                 
							T1 = conversion_fun(jt1[i])*10.0  
							lt1.append(T1)
							
							"""detector diode & filter """
							T2 = conversion_fun(jt2[i])*10.0 
							lt2.append(T2)  
							
							"""ambient at feed"""
							T3 = conversion_fun(jt3[i])*10.0  
							lt3.append(T3)
							
							"""electronics box"""
							T4 = conversion_fun(jt4[i])*10.0 
							lt4.append(T4)
							
							#::::::::: Noise source ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
							Tns = 333.0-(conversion_fun(jtns[i]))*46.29 #in Celsius 
							Vns = conversion_fun(jvns[i])	      # broadcast signal in V 
							ltns.append(Tns)
							lvns.append(Vns)
							
							#::::::::: Heaters :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
							HI = conversion_fun(jhi[i]) # heater voltage in Volts
							lhi.append(HI)
							
							#::::::::: Time ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
							if (file_name_index == 0) or (jfini_list[file_name_index] - prevs_jffin[z] != 1):
								timefra = (k - 1)*0.56002
								time_file = timeini + timefra
							else:
								timefra = (float(jframe[i]) - float(jffin_list[file_name_index]))*0.56002	
								time_file = timeini + timefra
							if time_file >= 86400.0 :
								time_file = time_file -86400.0
							time_t.append(time_file)
							
						#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
							data6 = str(jframe[i])
							for item in [time_t[i], elt[i], sig[i], azi[i], lt1[i], lt2[i], lt3[i], lt4[i],\
							lhi[i], ltns[i], lvns[i]]:
								item = f"{item:.3f}"
								data6 += f" {item} "
							new_name = file_name.replace(".txt", ".dat")
							if i == 0:
								with open(Dir6 + new_name, "w") as arq7:
									arq7.write(data6)
									arq7.write("\n")
							else:
								with open(Dir6 + new_name, "a") as arq12:
									arq12.write(data6)
									arq12.write("\n")	
							
					#::::: Temp stats ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
					avg_temps, temp_sig, delta = temp_stats_avg(lt1, lt2, lt3, lt4)
					datatemps = ""
					for i in range(3):
						for item in [avg_temps[i], temp_sig[i], delta[i]]:
							np.set_printoptions(suppress=True, precision=3)
							#item = f"{item:.3f}"
							datatemps += f" {item}\t"
										
					fname_temps = file_list.replace(".txt", "tem.dat")
					if file_name_index == 0:
						with open(Dir4 + fname_temps, "w") as arq20:
							arq20.write("T1\tST1\tDT1\tT2\tST2\tDT2\tT3\tST3\tDT3\tT4\tST4\tDT4\n")
							arq20.write(datatemps)
							arq20.write("\n")
					else:
						with open(Dir4 + fname_temps, "a") as arq20:
							arq20.write(datatemps)
							arq20.write("\n")	
					
			#::::::::::::::::::: End of file_loop :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
				
			if jlist == 1:
				mdtime	= 1.0
			else:
				mdtime	= mdtime/(lm*1.0)
				sdtime	= 0.0	
				for i in range(lm):
					sdtime	= sdtime + (dtime[i] - mdtime)**2.0  ####
				#sdtime	= math.sqrt(sdtime/(lm - 1)) 
				""" new time offset"""
				tioff += mdtime 	
				
			data8 = str(tioff)
			for item in [mdtime, sdtime, mit]:
				item = f" {item:.3f} "
				data8 += f"\t{item}"
			with open(Dir2 + file_list, "w") as arq13:
				arq13.write("tioff\tmdtime\tsdtime\tmit\n")
				arq13.write(data8)
				
	#::::::::::: End offset_lop :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#----------------------------------------------- END CONVERSION -------------------------------------------------------------------------
"FUNCTIONS => "
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
def temp_stats_avg(t1_list, t2_list, t3_list, t4_list):
	
	ord_t1 = np.array(heap_sort(t1_list)) # ordering the temperature values
	ord_t2 = np.array(heap_sort(t2_list))
	ord_t3 = np.array(heap_sort(t3_list))
	ord_t4 = np.array(heap_sort(t4_list))
	k = len(ord_t1)
	avg_t1, avg_t2, avg_t3, avg_t4 = 0, 0, 0, 0
	sig_avg1, sig_avg2, sig_avg3, sig_avg4 = 0, 0, 0, 0
	 
	for i in range(k):
		avg_t1 += ord_t1[i] 
		avg_t2 += ord_t2[i]
		avg_t3 += ord_t3[i] 
		avg_t4 += ord_t4[i] 
	b = 0 
	for sorted_array in [ord_t1, ord_t2, ord_t3, ord_t4]:
		if b == 0:
			dlt = np.array(((sorted_array[-1] - sorted_array[0])/2))
		else:
			cal = np.array(((sorted_array[-1] - sorted_array[0])/2))
			dlt = np.hstack((dlt, cal))
		b += 1 
		
	delta = dlt
	print("delta", delta)	
	
	for i in [avg_t1, avg_t2, avg_t3, avg_t4]:
		if i == avg_t1:
			avg_temps = np.array(i/k)
		else:
			beta = np.array(i/k)
			avg_temps = np.hstack((avg_temps, beta))
		
	print("avgtemps", avg_temps)	
	
	for i in range(k):
		sig_avg1 += (ord_t1[i] - avg_temps[0])**2
		sig_avg2 += (ord_t2[i] - avg_temps[1])**2
		sig_avg3 += (ord_t3[i]- avg_temps[2])**2
		sig_avg4 += (ord_t4[i]- avg_temps[3])**2
	
	for sigma in [sig_avg1, sig_avg2, sig_avg3, sig_avg4]:
		if sigma == sig_avg1:
			sig_t = np.array(math.sqrt(float(sigma)/(k-1))*(3600/(k*0.56002)))
		else:
			alpha = np.array(math.sqrt(float(sigma)/(k-1))*(3600/(k*0.56002)))
			sig_t = np.hstack((sig_t, alpha))	
		
	temp_sig = sig_t
	print("tempsig", temp_sig)
	
	return(avg_temps, temp_sig, delta)
		
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
def conversion_fun(jdu):
	'''
	output -> float in physical units
	'''					
	jbits = 2**15.0
	du_V = (10.0*float(float(jdu) - jbits))/float(jbits)
	
	return(du_V)				
					
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Function to perform the sorting using
# heaop sort
def heap_sort(arr):
	hp.heapify(arr)
	result = []
	while arr:
		result.append(hp.heappop(arr))
	return result

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
main()
