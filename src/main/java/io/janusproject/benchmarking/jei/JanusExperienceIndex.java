/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2012-2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject.benchmarking.jei;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.net.InetSocketAddress;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Properties;
import java.util.Random;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.crypto.Cipher;
import javax.crypto.spec.DESKeySpec;
import javax.crypto.spec.SecretKeySpec;

import org.arakhne.afc.vmutil.locale.Locale;

/** This class computes the Janus Experience Index (JEI) of your machine.
 * <p>
 * The JEI is inspirated by the Windows Experience Index.
 * The JEI measures the capability of your computer's hardware and 
 * software configuration and expresses this measurement as a number
 * called a base score. A higher base score generally means that your
 * computer will perform better and faster than a computer with a
 * lower base score.
 * 
 * <h2>Global Base Score</h2>
 * 
 * The base score is a value between 1.0 and 5.9.
 * The base score is based on the sub scores of sub components. 
 * <p>
 * The model logic is tolerant of one subscore being below the threshold for
 * a particular level by 0.1.  For example, assume that in the above example,
 * the memory score was 4.0 and the processor score 3.9. 
 * This would mean that the processor score would marginally be the only item 
 * keeping the base score below level 4. The model addresses this issue by 
 * rounding up a single value that is below the next round level by 0.1. 
 * <p>
 * After the sub scores are normalized, the global base score is the average
 * score of the sub components.
 * 
 * <h2>CPU Score</h2>
 * 
 * The CPU score was created to measure the processor performance when tasked with common Janus
 * usage activities. The processor is assessed on the following items:<ol>
 * <li>Compression and decompression using the Zip compression algorithm</li>
 * <li>Encryption and decryption assessment</li>
 * <li>Arithmetic Operations</li>
 * </ol>
 * The results are normalized and weight averaged in order to arrive at the final CPU sub score.
 * 
 * <h2>Memory Score</h2>
 * 
 * The memory score measures the bandwidth of moving data into and out of memory in 
 * Mega Bytes per Second. The higher the bandwidth, the better the memory.
 * Not having enough memory is a limiting factor on performance. As a result, 
 * the amount of memory in the system constrains the score value.
 * The amount of memory limits are:
 * <table>
 * <thead>
 * <tr><th>Amount of memory</th><th>Highest possible score</th></tr>
 * </thead>
 * <tbody>
 * <tr><td>64 MB or less</td><td>1.0</td></tr>
 * <tr><td>Less than 128 MB</td><td>2.0</td></tr>
 * <tr><td>Less than 256 MB</td><td>3.0</td></tr>
 * <tr><td>Less than 512 MB</td><td>4.0</td></tr>
 * <tr><td>Less than 1 GB</td><td>5.0</td></tr>
 * </tbody>
 * </table>
 * 
 * <h2>Disk Score</h2>
 * 
 * The disk score measures disk bandwidth (in Mega Bytes per Second). The conversion 
 * to an index number is set up in a way that all modern disks will score at least 2.0.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusExperienceIndex {

	/** Version of the JEI.
	 */
	public static final String VERSION = "1.0"; //$NON-NLS-1$
	
	private static final float LOWER_COMPRESSION_DELAY = secondToNanosecond(3);
	private static final float LOWER_ENCRYPTION_DELAY = secondToNanosecond(4);
	private static final float LOWER_ARITHMETIC_DELAY = secondToNanosecond(30);
	private static final float LOWER_MEMORY_DELAY = secondToNanosecond(1);
	private static final float LOWER_DISK_DELAY = secondToNanosecond(5);
	private static final int HIGHER_PROCESSOR_COUNT = 16;
	
	private static final JEI JEI_SINGLETON = new JEI();

	private static float secondToNanosecond(float s) {
		return s*(1000*1000*1000);
	}
	
	private static String wrap(float v) {
		if (Float.isNaN(v))
			return Locale.getString("NC"); //$NON-NLS-1$
		return Float.toString(v);
	}
	
	/**
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		System.out.println(Locale.getString("COMPUTING_SCORES", VERSION)); //$NON-NLS-1$
		JEI jei = janusExperienceIndex();
		System.out.println(Locale.getString("GLOBAL_BASE_SCORE", wrap(jei.getBaseScore()))); //$NON-NLS-1$
		System.out.println(Locale.getString("CPU_SCORE", wrap(jei.getCpuScore()))); //$NON-NLS-1$
		System.out.println(Locale.getString("MEMORY_SCORE", wrap(jei.getMemoryScore()))); //$NON-NLS-1$
		System.out.println(Locale.getString("DISK_SCORE", wrap(jei.getDiskScore()))); //$NON-NLS-1$
	}
	
	/** Replies the current JEI.
	 * 
	 * @return the current JEI.
	 */
	public static JEI janusExperienceIndex() {
		baseScore();
		return JEI_SINGLETON;
	}
	
	/** Compute the global JEI.
	 * @return the global JEI.
	 */
	public static float baseScore() {
		float score = JEI_SINGLETON.getBaseScore();
		if (Float.isNaN(score)) {
			// Ensure sub scores are computed
			float[] scores = new float[] {
					cpuScore(),
					memoryScore(),
					diskScore(),
			};
			
			normalize(scores);
			score = avg(scores);

			JEI_SINGLETON.setCpuScore(scores[0]);
			JEI_SINGLETON.setMemoryScore(scores[1]);
			JEI_SINGLETON.setDiskScore(scores[2]);
			JEI_SINGLETON.setBaseScore(score);
		}		
		return score;
	}
	
	private static float clamp(float v, float min, float max) {
		if (v<min) return min;
		if (v>max) return max;
		return v;
	}
	
	private static float normalize(float f) {
		if (Float.isNaN(f)) return Float.NaN;
		return Math.round(f * 10f) / 10f;
	}
	
	private static void normalize(float[] scores) {
		for(int i=0; i<scores.length; ++i) {
			float s = scores[i];
			if (!Float.isNaN(s)) {
				for(int j=0; j<scores.length; ++j) {
					if (j!=i) {
						float score = scores[j];
						if (s>=(score-.1f) && s<=score) {
							s = score;
						}
					}
				}
			}
			scores[i] = s;
		}
	}

	private static float avg(float[] scores) {
		int n = 0;
		float score = 0f;
		for(float subscore : scores) {
			if (!Float.isNaN(subscore)) {
				score += subscore;
				++n;
			}
		}
		if (n>0) {
			score /= n;
			score = normalize(score);
		}
		else {
			score = Float.NaN;
		}
		return score;
	}

	private static void garbage() {
		Runtime r = Runtime.getRuntime();
		for(int i=0; i<6; ++i) {
			r.gc();
		}
	}

	/** Compute the CPU score.
	 * @return the CPU score.
	 */
	public static float cpuScore() {
		float score = JEI_SINGLETON.getCpuScore();
		if (Float.isNaN(score)) {
			float[] scores = new float[] {
					computeCompressionScore(),
					computeEncryptionScore(),
					computeArithmeticScore(),
					computeMultiProcessorScore()
			};
			
			score = avg(scores);
			
			JEI_SINGLETON.setCpuScore(normalize(score));
		}		
		return score;
	}	
	
	private static float computeMultiProcessorScore() {
		try {
			Runtime r = Runtime.getRuntime();
			int n = r.availableProcessors();
			return clamp(n * 5f / HIGHER_PROCESSOR_COUNT + 1f, 1, 6);
		}
		catch(Exception e) {
			return Float.NaN;
		}
	}

	private static float computeArithmeticScore() {
		try {
			float arithDelay;
			{
				long s, e;
				s = System.nanoTime();
				for(long i=0; i<100000000; ++i) {
					Math.atan2(123, 456);
				}
				e = System.nanoTime();
				
				arithDelay = e - s;
			}

			garbage();

			float score = arithDelay * 5f / LOWER_ARITHMETIC_DELAY;
			score = 6 - score;
			
			return clamp(score, 1, 6);
		}
		catch(Exception e) {
			return Float.NaN;
		}
	}

	private static float computeEncryptionScore() {
		try {
			long s, e;
			float encryptionDelay;
			byte[] buffer;
			{
				Random rnd = new Random();
				StringBuilder t = new StringBuilder();
				for(long i=0; i<6000; ++i) {
					for(int j=0; j<1024; ++j) {
						t.append((char)rnd.nextInt());
					}
				}
				t.trimToSize();
				buffer = t.toString().getBytes();
			}

			garbage();

			{
				Properties p = System.getProperties();
				String seed =	p.getProperty("user.name") //$NON-NLS-1$
									+'@'
									+new InetSocketAddress(0).getHostName()
									+":jdk_" //$NON-NLS-1$
									+p.getProperty("java.version") //$NON-NLS-1$
									+":os_" //$NON-NLS-1$
									+p.getProperty("os.name") //$NON-NLS-1$
									+'-'
									+p.getProperty("os.version"); //$NON-NLS-1$
				byte[] original = md5(seed).getBytes("UTF8"); //$NON-NLS-1$
				byte[] output;
				byte[] bkey = new byte[DESKeySpec.DES_KEY_LEN];
				for(int i=0; i<DESKeySpec.DES_KEY_LEN; ++i) {
					bkey[i] = original[i%original.length];
				}
				
				Key kkey = new SecretKeySpec(
						bkey,
						"DES"); //$NON-NLS-1$
				Cipher cipher = Cipher.getInstance("DES"); //$NON-NLS-1$
				
				s = System.nanoTime();
		        //MD5
				MessageDigest.getInstance("MD5").digest(buffer); //$NON-NLS-1$
		        //SHA
				MessageDigest.getInstance("SHA").digest(buffer); //$NON-NLS-1$
				//DES Encryption
				cipher.init(Cipher.ENCRYPT_MODE, kkey);
				output = cipher.doFinal(buffer);
				//DES Decryption
				cipher.init(Cipher.DECRYPT_MODE, kkey);
				cipher.doFinal(output);
		        e = System.nanoTime();
		        
		        encryptionDelay = e - s;
			}

			garbage();

			float score = encryptionDelay * 5f / LOWER_ENCRYPTION_DELAY;
			score = 6 - score;
			
			return clamp(score, 1, 6);
		}
		catch(Exception e) {
			return Float.NaN;
		}
	}
	
	private static String md5(String str) {
		if (str==null) return "";  //$NON-NLS-1$
        byte[] uniqueKey = str.getBytes();
        byte[] hash = null;
        
        try {
        	hash = MessageDigest.getInstance("MD5").digest(uniqueKey); //$NON-NLS-1$
        }
        catch (NoSuchAlgorithmException e) {
        	throw new Error(Locale.getString("NO_MD5")); //$NON-NLS-1$
        }
        
        StringBuilder hashString = new StringBuilder();
        
        for ( int i = 0; i < hash.length; ++i ) {
	        String hex = Integer.toHexString(hash[i]);
	        if ( hex.length() == 1 ) {
	            hashString.append('0');
	            hashString.append(hex.charAt(hex.length()-1));
	        } else {
	            hashString.append(hex.substring(hex.length()-2));
	        }
        }
        return hashString.toString();		
	}

	private static float computeCompressionScore() {
		try {
			long s, e;
			float compressionDelay;
			byte[] buffer;
			{
				Random rnd = new Random();
				StringBuilder t = new StringBuilder();
				for(long i=0; i<6000; ++i) {
					for(int j=0; j<1024; ++j) {
						t.append((char)rnd.nextInt());
					}
				}
				t.trimToSize();
				buffer = t.toString().getBytes();
			}

			garbage();

			try(ZipOutputStream zos = new ZipOutputStream(new ByteArrayOutputStream())) {
				s = System.nanoTime();
				zos.putNextEntry(new ZipEntry("test.bin")); //$NON-NLS-1$
				zos.write(buffer, 0, buffer.length);
				e = System.nanoTime();
				compressionDelay = e - s;
			}

			garbage();

			float score = compressionDelay * 5f / LOWER_COMPRESSION_DELAY;
			score = 6 - score;
			
			return clamp(score, 1, 6);
		}
		catch(Exception e) {
			return Float.NaN;
		}
	}
	
	private static float megaBytes(float m) {
		return m*1024*1024;
	}
	
	/** Compute the Memory score.
	 * @return the Memory score.
	 */
	@SuppressWarnings("unused")
	public static float memoryScore() {
		float score = JEI_SINGLETON.getMemoryScore();
		if (Float.isNaN(score)) {
			long e, s;
			
			{
				byte t;
				s = System.nanoTime();
				byte[] tab = new byte[(int)megaBytes(10)];
				for(int i=0; i<tab.length; ++i) {
					tab[i] = 123;
					if (i>0) t = tab[i-1];
					if (i<(tab.length-1)) t = tab[i+1];
				}
				for(int i=0; i<10000; ++i) {
					new String("ABSD"); //$NON-NLS-1$
				}
				e = System.nanoTime();
			}
			
			garbage();
			
			score = (e-s) * 5f / LOWER_MEMORY_DELAY;
			score = 6 - score;

			Runtime r = Runtime.getRuntime();
			if (r.maxMemory()<=megaBytes(64)) score = 1f;
			else if (r.maxMemory()<=megaBytes(128)) score = clamp(score, 1, 2);
			else if (r.maxMemory()<=megaBytes(256)) score = clamp(score, 1, 3);
			else if (r.maxMemory()<=megaBytes(512)) score = clamp(score, 1, 4);
			else if (r.maxMemory()<=megaBytes(1024)) score = clamp(score, 1, 5);
			else score = clamp(score, 1, 6);
			
			JEI_SINGLETON.setMemoryScore(normalize(score));
		}		
		return score;
	}	

	/** Compute the disk score.
	 * @return the disk score.
	 */
	public static float diskScore() {
		float score = JEI_SINGLETON.getDiskScore();
		if (Float.isNaN(score)) {
			try {
				File tempFile = File.createTempFile("jei", ".bin"); //$NON-NLS-1$ //$NON-NLS-2$
				tempFile.deleteOnExit();
				long e, s;
					
				s = System.nanoTime();

				try(FileWriter fw = new FileWriter(tempFile)) {
					for(int i=0; i<1024*1024*20; ++i) {
						fw.write('A');
					}
					fw.flush();
				}
				
				try(FileReader fr = new FileReader(tempFile)) {
					while (fr.read()!=-1) {
						//
					}
				}

				e = System.nanoTime();

				tempFile.delete();
				score = (e-s) * 5f / LOWER_DISK_DELAY;
				score = 6 - score;
				score = clamp(score, 1, 6);
			}
			catch(Exception _) {
				score = Float.NaN;
			}
			JEI_SINGLETON.setDiskScore(normalize(score));
		}		
		return score;
	}	

	/** This class contains the different JEI of your machine.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class JEI {
		
		private float base = Float.NaN;
		private float cpu = Float.NaN;
		private float memory = Float.NaN;
		private float disk = Float.NaN;
		
		/**
		 */
		JEI() {
			//
		}
		
		/** Set the base score.
		 * 
		 * @param score
		 */
		void setBaseScore(float score) {
			this.base = score;
		}
		
		/** Replies the base score.
		 * 
		 * @return the base score, or {@link Float#NaN} if
		 * the score was not computed.
		 */
		public float getBaseScore() {
			return this.base;
		}

		/** Set the CPU score.
		 * 
		 * @param score
		 */
		void setCpuScore(float score) {
			this.cpu = score;
		}
		
		/** Replies the CPU score.
		 * 
		 * @return the CPU score, or {@link Float#NaN} if
		 * the score was not computed.
		 */
		public float getCpuScore() {
			return this.cpu;
		}

		/** Set the Memory score.
		 * 
		 * @param score
		 */
		void setMemoryScore(float score) {
			this.memory = score;
		}
		
		/** Replies the Memory score.
		 * 
		 * @return the memory score, or {@link Float#NaN} if
		 * the score was not computed.
		 */
		public float getMemoryScore() {
			return this.memory;
		}

		/** Set the disk score.
		 * 
		 * @param score
		 */
		void setDiskScore(float score) {
			this.disk = score;
		}
		
		/** Replies the disk score.
		 * 
		 * @return the disk score, or {@link Float#NaN} if
		 * the score was not computed.
		 */
		public float getDiskScore() {
			return this.disk;
		}

	}

}