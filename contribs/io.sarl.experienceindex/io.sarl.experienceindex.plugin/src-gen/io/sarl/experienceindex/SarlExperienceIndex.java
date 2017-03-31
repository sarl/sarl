/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2017 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.experienceindex;

import io.sarl.experienceindex.Messages;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.lang.reflect.Array;
import java.net.InetSocketAddress;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.crypto.Cipher;
import javax.crypto.spec.DESKeySpec;
import javax.crypto.spec.SecretKeySpec;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * This class computes the SARL Experience Index (SEI) of your machine.
 * 
 * <p>The SEI is inspirated by the <a href="https://en.wikipedia.org/wiki/Windows_System_Assessment_Tool">Windows
 * Experience Index</a>.
 * The SEI measures the capability of your computer's hardware and
 * software configuration and expresses this measurement as a number
 * called a base score. A higher base score generally means that your
 * computer will perform better and faster than a computer with a
 * lower base score.
 * 
 * <h2>Global Base Score</h2>
 * 
 * <p>The base score is a value between 1.0 and 5.9.
 * The base score is based on the sub scores of sub components.
 * 
 * <p>The model logic is tolerant of one subscore being below the threshold for
 * a particular level by 0.1.  For example, assume that in the above example,
 * the memory score was 4.0 and the processor score 3.9.
 * This would mean that the processor score would marginally be the only item
 * keeping the base score below level 4. The model addresses this issue by
 * rounding up a single value that is below the next round level by 0.1.
 * 
 * <p>After the sub scores are normalized, the global base score is the average
 * score of the sub components.
 * 
 * <h2>CPU Score</h2>
 * 
 * <p>The CPU score was created to measure the processor performance when tasked with common Janus
 * usage activities. The processor is assessed on the following items:<ol>
 * <li>Compression and decompression using the Zip compression algorithm</li>
 * <li>Encryption and decryption assessment</li>
 * <li>Arithmetic Operations</li>
 * </ol>
 * The results are normalized and weight averaged in order to arrive at the final CPU sub score.
 * 
 * <h2>Memory Score</h2>
 * 
 * <p>The memory score measures the bandwidth of moving data into and out of memory in
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
 * <p>The disk score measures disk bandwidth (in Mega Bytes per Second). The conversion
 * to an index number is set up in a way that all modern disks will score at least 2.0.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SarlSpecification("0.6")
@SuppressWarnings("all")
public final class SarlExperienceIndex {
  /**
   * This class contains the different SEI of your machine.
   * 
   * @author $Author: sgalland$
   * @version $FullVersion$
   * @mavengroupid $GroupId$
   * @mavenartifactid $ArtifactId$
   */
  @SarlSpecification("0.6")
  public static class SEI {
    private float base = Float.NaN;
    
    private float cpu = Float.NaN;
    
    private float memory = Float.NaN;
    
    private float disk = Float.NaN;
    
    SEI() {
    }
    
    /**
     * Set the base score.
     * 
     * @param score
     */
    void setBaseScore(final float score) {
      this.base = score;
    }
    
    /**
     * Replies the base score.
     * 
     * @return the base score, or {@link Float#NaN} if
     * the score was not computed.
     */
    @Pure
    public float getBaseScore() {
      return this.base;
    }
    
    /**
     * Set the CPU score.
     * 
     * @param score
     */
    void setCpuScore(final float score) {
      this.cpu = score;
    }
    
    /**
     * Replies the CPU score.
     * 
     * @return the CPU score, or {@link Float#NaN} if
     * the score was not computed.
     */
    @Pure
    public float getCpuScore() {
      return this.cpu;
    }
    
    /**
     * Set the Memory score.
     * 
     * @param score
     */
    void setMemoryScore(final float score) {
      this.memory = score;
    }
    
    /**
     * Replies the Memory score.
     * 
     * @return the memory score, or {@link Float#NaN} if
     * the score was not computed.
     */
    @Pure
    public float getMemoryScore() {
      return this.memory;
    }
    
    /**
     * Set the disk score.
     * 
     * @param score
     */
    void setDiskScore(final float score) {
      this.disk = score;
    }
    
    /**
     * Replies the disk score.
     * 
     * @return the disk score, or {@link Float#NaN} if
     * the score was not computed.
     */
    @Pure
    public float getDiskScore() {
      return this.disk;
    }
    
    @Override
    @Pure
    @SyntheticMember
    public boolean equals(final Object obj) {
      if (this == obj)
        return true;
      if (obj == null)
        return false;
      if (getClass() != obj.getClass())
        return false;
      SEI other = (SEI) obj;
      if (Float.floatToIntBits(other.base) != Float.floatToIntBits(this.base))
        return false;
      if (Float.floatToIntBits(other.cpu) != Float.floatToIntBits(this.cpu))
        return false;
      if (Float.floatToIntBits(other.memory) != Float.floatToIntBits(this.memory))
        return false;
      if (Float.floatToIntBits(other.disk) != Float.floatToIntBits(this.disk))
        return false;
      return super.equals(obj);
    }
    
    @Override
    @Pure
    @SyntheticMember
    public int hashCode() {
      final int prime = 31;
      int result = super.hashCode();
      result = prime * result + Float.floatToIntBits(this.base);
      result = prime * result + Float.floatToIntBits(this.cpu);
      result = prime * result + Float.floatToIntBits(this.memory);
      result = prime * result + Float.floatToIntBits(this.disk);
      return result;
    }
  }
  
  /**
   * Version of the SEI.
   */
  public final static String VERSION = "1.0";
  
  private final static float LOWER_COMPRESSION_DELAY = SarlExperienceIndex.secs(3);
  
  private final static float LOWER_ENCRYPTION_DELAY = SarlExperienceIndex.secs(4);
  
  private final static float LOWER_ARITHMETIC_DELAY = SarlExperienceIndex.secs(30);
  
  private final static float LOWER_MEMORY_DELAY = SarlExperienceIndex.secs(1);
  
  private final static float LOWER_DISK_DELAY = SarlExperienceIndex.secs(5);
  
  private final static int HIGHER_PROCESSOR_COUNT = 16;
  
  private final static float NS = ((1000f * 1000f) * 1000f);
  
  private static strictfp float secs(final float s) {
    return (s * SarlExperienceIndex.NS);
  }
  
  private static strictfp float megaBytes(final float m) {
    return ((m * 1024) * 1024);
  }
  
  private static strictfp String wrap(final float v) {
    String _xifexpression = null;
    boolean _isNaN = Float.isNaN(v);
    if (_isNaN) {
      _xifexpression = Messages.SarlExperienceIndex_0;
    } else {
      _xifexpression = Float.toString(v);
    }
    return _xifexpression;
  }
  
  private static strictfp float clamp(final float v, final float min, final float max) {
    if ((v < min)) {
      return min;
    }
    if ((v > max)) {
      return max;
    }
    return v;
  }
  
  private static strictfp float normalize(final float value) {
    float _xblockexpression = (float) 0;
    {
      boolean _isNaN = Float.isNaN(value);
      if (_isNaN) {
        return Float.NaN;
      }
      int _round = Math.round((value * 10f));
      _xblockexpression = (_round / 10f);
    }
    return _xblockexpression;
  }
  
  private static strictfp void normalize(final float... scores) {
    for (int i = 0; (i < scores.length); i++) {
      {
        float s = scores[i];
        boolean _isNaN = Float.isNaN(s);
        boolean _not = (!_isNaN);
        if (_not) {
          for (int j = 0; (j < scores.length); j++) {
            if ((j != i)) {
              float score = scores[j];
              if (((s >= (score - 0.1f)) && (s <= score))) {
                s = score;
              }
            }
          }
        }
        scores[i] = s;
      }
    }
  }
  
  private static strictfp float avg(final float... scores) {
    int n = 0;
    float score = 0f;
    for (final float subscore : scores) {
      boolean _isNaN = Float.isNaN(subscore);
      boolean _not = (!_isNaN);
      if (_not) {
        float _score = score;
        score = (_score + subscore);
        n++;
      }
    }
    if ((n > 0)) {
      float _score_1 = score;
      score = (_score_1 / n);
      score = SarlExperienceIndex.normalize(score);
    } else {
      score = Float.NaN;
    }
    return score;
  }
  
  private static strictfp void garbage() {
    final Runtime r = Runtime.getRuntime();
    for (int i = 0; (i < 6); i++) {
      r.gc();
    }
  }
  
  private final static SarlExperienceIndex.SEI SEI_SINGLETON = new SarlExperienceIndex.SEI();
  
  private SarlExperienceIndex() {
  }
  
  /**
   * Run the SEI from the command-line.
   * 
   * @param args no used.
   * @throws Exception in case of error.
   */
  @SuppressWarnings("discouraged_reference")
  public static strictfp void main(final String... args) {
    InputOutput.<String>println(MessageFormat.format(Messages.SarlExperienceIndex_1, SarlExperienceIndex.VERSION));
    final SarlExperienceIndex.SEI SEI = SarlExperienceIndex.getJanusExperienceIndex();
    InputOutput.<String>println(MessageFormat.format(Messages.SarlExperienceIndex_2, SarlExperienceIndex.wrap(SEI.getBaseScore())));
    InputOutput.<String>println(MessageFormat.format(Messages.SarlExperienceIndex_3, Float.valueOf(SEI.getCpuScore())));
    InputOutput.<String>println(MessageFormat.format(Messages.SarlExperienceIndex_4, Float.valueOf(SEI.getMemoryScore())));
    InputOutput.<String>println(MessageFormat.format(Messages.SarlExperienceIndex_5, Float.valueOf(SEI.getDiskScore())));
    InputOutput.<String>println(Messages.SarlExperienceIndex_6);
  }
  
  /**
   * Replies the current SEI.
   * 
   * @return the current SEI.
   */
  @Pure
  public static strictfp SarlExperienceIndex.SEI getJanusExperienceIndex() {
    SarlExperienceIndex.SEI _xblockexpression = null;
    {
      SarlExperienceIndex.baseScore();
      _xblockexpression = SarlExperienceIndex.SEI_SINGLETON;
    }
    return _xblockexpression;
  }
  
  /**
   * Compute the global SEI.
   * 
   * @return the global SEI.
   */
  public static strictfp float baseScore() {
    float score = SarlExperienceIndex.SEI_SINGLETON.getBaseScore();
    boolean _isNaN = Float.isNaN(score);
    if (_isNaN) {
      float _cpuScore = SarlExperienceIndex.cpuScore();
      float _memoryScore = SarlExperienceIndex.memoryScore();
      float _diskScore = SarlExperienceIndex.diskScore();
      final List<Float> scores = Collections.<Float>unmodifiableList(CollectionLiterals.<Float>newArrayList(Float.valueOf(_cpuScore), Float.valueOf(_memoryScore), Float.valueOf(_diskScore)));
      SarlExperienceIndex.normalize(((float[])Conversions.unwrapArray(scores, float.class)));
      score = SarlExperienceIndex.avg(((float[])Conversions.unwrapArray(scores, float.class)));
      SarlExperienceIndex.SEI_SINGLETON.setCpuScore((scores.get(0)).floatValue());
      SarlExperienceIndex.SEI_SINGLETON.setMemoryScore((scores.get(1)).floatValue());
      SarlExperienceIndex.SEI_SINGLETON.setDiskScore((scores.get(2)).floatValue());
      SarlExperienceIndex.SEI_SINGLETON.setBaseScore(score);
    }
    return score;
  }
  
  /**
   * Compute the CPU score.
   * 
   * @return the CPU score.
   */
  public static strictfp float cpuScore() {
    float score = SarlExperienceIndex.SEI_SINGLETON.getCpuScore();
    boolean _isNaN = Float.isNaN(score);
    if (_isNaN) {
      float _computeCompressionScore = SarlExperienceIndex.computeCompressionScore();
      float _computeEncryptionScore = SarlExperienceIndex.computeEncryptionScore();
      float _computeArithmeticScore = SarlExperienceIndex.computeArithmeticScore();
      float _computeMultiProcessorScore = SarlExperienceIndex.computeMultiProcessorScore();
      List<Float> scores = Collections.<Float>unmodifiableList(CollectionLiterals.<Float>newArrayList(Float.valueOf(_computeCompressionScore), Float.valueOf(_computeEncryptionScore), Float.valueOf(_computeArithmeticScore), Float.valueOf(_computeMultiProcessorScore)));
      final List<Float> _converted_scores = (List<Float>)scores;
      score = SarlExperienceIndex.avg(((float[])Conversions.unwrapArray(_converted_scores, float.class)));
      score = SarlExperienceIndex.normalize(score);
      SarlExperienceIndex.SEI_SINGLETON.setCpuScore(score);
    }
    return score;
  }
  
  private static strictfp float computeMultiProcessorScore() {
    float _xtrycatchfinallyexpression = (float) 0;
    try {
      float _xblockexpression = (float) 0;
      {
        Runtime r = Runtime.getRuntime();
        int n = r.availableProcessors();
        _xblockexpression = SarlExperienceIndex.clamp((((n * 5f) / SarlExperienceIndex.HIGHER_PROCESSOR_COUNT) + 1f), 1, 6);
      }
      _xtrycatchfinallyexpression = _xblockexpression;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        _xtrycatchfinallyexpression = Float.NaN;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
  
  private static strictfp float computeArithmeticScore() {
    float _xtrycatchfinallyexpression = (float) 0;
    try {
      float _xblockexpression = (float) 0;
      {
        long s = System.nanoTime();
        for (int i = 0; (i < 100000000); i++) {
          Math.atan2(123, 456);
        }
        long e = System.nanoTime();
        long arithDelay = (e - s);
        SarlExperienceIndex.garbage();
        float score = ((arithDelay * 5f) / SarlExperienceIndex.LOWER_ARITHMETIC_DELAY);
        score = (6 - score);
        _xblockexpression = SarlExperienceIndex.clamp(score, 1, 6);
      }
      _xtrycatchfinallyexpression = _xblockexpression;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        _xtrycatchfinallyexpression = Float.NaN;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
  
  private static strictfp float computeEncryptionScore() {
    float _xtrycatchfinallyexpression = (float) 0;
    try {
      float _xblockexpression = (float) 0;
      {
        Random rnd = new Random();
        StringBuilder t = new StringBuilder();
        for (int i = 0; (i < 6000); i++) {
          for (int j = 0; (j < 1024); j++) {
            int _nextInt = rnd.nextInt();
            t.append(((char) _nextInt));
          }
        }
        t.trimToSize();
        byte[] buffer = t.toString().getBytes();
        SarlExperienceIndex.garbage();
        Properties p = System.getProperties();
        String _property = p.getProperty("user.name");
        String _plus = (_property + "@");
        String _hostName = new InetSocketAddress(0).getHostName();
        String _plus_1 = (_plus + _hostName);
        String _plus_2 = (_plus_1 + ":jdk_");
        String _property_1 = p.getProperty("java.version");
        String _plus_3 = (_plus_2 + _property_1);
        String _plus_4 = (_plus_3 + ":os_");
        String _property_2 = p.getProperty("os.name");
        String _plus_5 = (_plus_4 + _property_2);
        String _plus_6 = (_plus_5 + "-");
        String _property_3 = p.getProperty("os.version");
        String seed = (_plus_6 + _property_3);
        byte[] original = SarlExperienceIndex.md5(seed).getBytes("UTF8");
        Object _newInstance = Array.newInstance(byte.class, DESKeySpec.DES_KEY_LEN);
        byte[] bkey = ((byte[]) _newInstance);
        for (int i = 0; (i < DESKeySpec.DES_KEY_LEN); i++) {
          int _length = original.length;
          int _modulo = (i % _length);
          bkey[i] = original[_modulo];
        }
        SecretKeySpec kkey = new SecretKeySpec(bkey, "DES");
        Cipher cipher = Cipher.getInstance("DES");
        long s = System.nanoTime();
        MessageDigest.getInstance("MD5").digest(buffer);
        MessageDigest.getInstance("SHA").digest(buffer);
        cipher.init(Cipher.ENCRYPT_MODE, kkey);
        byte[] output = cipher.doFinal(buffer);
        cipher.init(Cipher.DECRYPT_MODE, kkey);
        cipher.doFinal(output);
        long e = System.nanoTime();
        long encryptionDelay = (e - s);
        SarlExperienceIndex.garbage();
        float score = ((encryptionDelay * 5f) / SarlExperienceIndex.LOWER_ENCRYPTION_DELAY);
        score = (6 - score);
        _xblockexpression = SarlExperienceIndex.clamp(score, 1, 6);
      }
      _xtrycatchfinallyexpression = _xblockexpression;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        _xtrycatchfinallyexpression = Float.NaN;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
  
  private static strictfp String md5(final String str) {
    String _xblockexpression = null;
    {
      if ((str == null)) {
        return "";
      }
      byte[] uniqueKey = str.getBytes();
      byte[] hash = null;
      try {
        hash = MessageDigest.getInstance("MD5").digest(uniqueKey);
      } catch (final Throwable _t) {
        if (_t instanceof NoSuchAlgorithmException) {
          final NoSuchAlgorithmException e = (NoSuchAlgorithmException)_t;
          throw new Error(Messages.SarlExperienceIndex_7, e);
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      StringBuilder hashString = new StringBuilder();
      for (int i = 0; (i < hash.length); i++) {
        {
          String hex = Integer.toHexString(hash[i]);
          int _length = hex.length();
          boolean _tripleEquals = (_length == 1);
          if (_tripleEquals) {
            hashString.append("0");
            int _length_1 = hex.length();
            int _minus = (_length_1 - 1);
            hashString.append(hex.charAt(_minus));
          } else {
            int _length_2 = hex.length();
            int _minus_1 = (_length_2 - 2);
            hashString.append(hex.substring(_minus_1));
          }
        }
      }
      _xblockexpression = hashString.toString();
    }
    return _xblockexpression;
  }
  
  private static strictfp float computeCompressionScore() {
    float _xtrycatchfinallyexpression = (float) 0;
    try {
      float _xblockexpression = (float) 0;
      {
        Random rnd = new Random();
        StringBuilder t = new StringBuilder();
        for (int i = 0; (i < 6000); i++) {
          for (int j = 0; (j < 1024); j++) {
            int _nextInt = rnd.nextInt();
            t.append(((char) _nextInt));
          }
        }
        t.trimToSize();
        byte[] buffer = t.toString().getBytes();
        SarlExperienceIndex.garbage();
        long compressionDelay = 0;
        ByteArrayOutputStream _byteArrayOutputStream = new ByteArrayOutputStream();
        ZipOutputStream zos = new ZipOutputStream(_byteArrayOutputStream);
        try {
          long s = System.nanoTime();
          ZipEntry _zipEntry = new ZipEntry("test.bin");
          zos.putNextEntry(_zipEntry);
          zos.write(buffer, 0, buffer.length);
          long e = System.nanoTime();
          compressionDelay = (e - s);
        } finally {
          zos.close();
        }
        SarlExperienceIndex.garbage();
        float score = ((compressionDelay * 5f) / SarlExperienceIndex.LOWER_COMPRESSION_DELAY);
        score = (6 - score);
        _xblockexpression = SarlExperienceIndex.clamp(score, 1, 6);
      }
      _xtrycatchfinallyexpression = _xblockexpression;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        _xtrycatchfinallyexpression = Float.NaN;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
  
  /**
   * Compute the Memory score.
   * 
   * @return the Memory score.
   */
  public static strictfp float memoryScore() {
    float score = SarlExperienceIndex.SEI_SINGLETON.getMemoryScore();
    boolean _isNaN = Float.isNaN(score);
    if (_isNaN) {
      long s = System.nanoTime();
      float _megaBytes = SarlExperienceIndex.megaBytes(10);
      Object _newInstance = Array.newInstance(byte.class, ((int) _megaBytes));
      byte[] tab = ((byte[]) _newInstance);
      for (int i = 0; (i < tab.length); i++) {
        {
          tab[i] = ((byte) 123);
          byte t = 0;
          if ((i > 0)) {
            t = tab[(i - 1)];
          }
          int _length = tab.length;
          int _minus = (_length - 1);
          boolean _lessThan = (i < _minus);
          if (_lessThan) {
            t = tab[(i + 1)];
          }
        }
      }
      for (int i = 0; (i < 10000); i++) {
        new String("ABSD");
      }
      long e = System.nanoTime();
      SarlExperienceIndex.garbage();
      score = (((e - s) * 5f) / SarlExperienceIndex.LOWER_MEMORY_DELAY);
      score = (6 - score);
      Runtime r = Runtime.getRuntime();
      long _maxMemory = r.maxMemory();
      float _megaBytes_1 = SarlExperienceIndex.megaBytes(64);
      boolean _lessEqualsThan = (_maxMemory <= _megaBytes_1);
      if (_lessEqualsThan) {
        score = 1f;
      } else {
        long _maxMemory_1 = r.maxMemory();
        float _megaBytes_2 = SarlExperienceIndex.megaBytes(128);
        boolean _lessEqualsThan_1 = (_maxMemory_1 <= _megaBytes_2);
        if (_lessEqualsThan_1) {
          score = SarlExperienceIndex.clamp(score, 1, 2);
        } else {
          long _maxMemory_2 = r.maxMemory();
          float _megaBytes_3 = SarlExperienceIndex.megaBytes(256);
          boolean _lessEqualsThan_2 = (_maxMemory_2 <= _megaBytes_3);
          if (_lessEqualsThan_2) {
            score = SarlExperienceIndex.clamp(score, 1, 3);
          } else {
            long _maxMemory_3 = r.maxMemory();
            float _megaBytes_4 = SarlExperienceIndex.megaBytes(512);
            boolean _lessEqualsThan_3 = (_maxMemory_3 <= _megaBytes_4);
            if (_lessEqualsThan_3) {
              score = SarlExperienceIndex.clamp(score, 1, 4);
            } else {
              long _maxMemory_4 = r.maxMemory();
              float _megaBytes_5 = SarlExperienceIndex.megaBytes(1024);
              boolean _lessEqualsThan_4 = (_maxMemory_4 <= _megaBytes_5);
              if (_lessEqualsThan_4) {
                score = SarlExperienceIndex.clamp(score, 1, 5);
              } else {
                score = SarlExperienceIndex.clamp(score, 1, 6);
              }
            }
          }
        }
      }
      score = SarlExperienceIndex.normalize(score);
      SarlExperienceIndex.SEI_SINGLETON.setMemoryScore(score);
    }
    return score;
  }
  
  /**
   * Compute the disk score.
   * 
   * @return the disk score.
   */
  public static strictfp float diskScore() {
    float score = SarlExperienceIndex.SEI_SINGLETON.getDiskScore();
    boolean _isNaN = Float.isNaN(score);
    if (_isNaN) {
      try {
        File tempFile = File.createTempFile("SEI", ".bin");
        tempFile.deleteOnExit();
        long s = System.nanoTime();
        FileWriter fw = new FileWriter(tempFile);
        try {
          for (int i = 0; (i < ((1024 * 1024) * 20)); i++) {
            fw.write("A");
          }
          fw.flush();
        } finally {
          fw.close();
        }
        FileReader fr = new FileReader(tempFile);
        try {
          while ((fr.read() != (-1))) {
          }
        } finally {
          fr.close();
        }
        long e = System.nanoTime();
        tempFile.delete();
        score = (((e - s) * 5f) / SarlExperienceIndex.LOWER_DISK_DELAY);
        score = (6 - score);
        score = SarlExperienceIndex.clamp(score, 1, 6);
        score = SarlExperienceIndex.normalize(score);
      } catch (final Throwable _t) {
        if (_t instanceof Exception) {
          final Exception exception = (Exception)_t;
          score = Float.NaN;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      SarlExperienceIndex.SEI_SINGLETON.setDiskScore(score);
    }
    return score;
  }
}
