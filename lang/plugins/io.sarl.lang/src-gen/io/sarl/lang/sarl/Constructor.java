/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Constructor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.Constructor#getParams <em>Params</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.Constructor#isVarargs <em>Varargs</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.Constructor#getBody <em>Body</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getConstructor()
 * @model
 * @generated
 */
public interface Constructor extends EventFeature, BehaviorFeature, SkillFeature
{
  /**
   * Returns the value of the '<em><b>Params</b></em>' containment reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.Parameter}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Params</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Params</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getConstructor_Params()
   * @model containment="true"
   * @generated
   */
  EList<Parameter> getParams();

  /**
   * Returns the value of the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Varargs</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Varargs</em>' attribute.
   * @see #setVarargs(boolean)
   * @see io.sarl.lang.sarl.SarlPackage#getConstructor_Varargs()
   * @model
   * @generated
   */
  boolean isVarargs();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.Constructor#isVarargs <em>Varargs</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Varargs</em>' attribute.
   * @see #isVarargs()
   * @generated
   */
  void setVarargs(boolean value);

  /**
   * Returns the value of the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Body</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Body</em>' containment reference.
   * @see #setBody(XExpression)
   * @see io.sarl.lang.sarl.SarlPackage#getConstructor_Body()
   * @model containment="true"
   * @generated
   */
  XExpression getBody();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.Constructor#getBody <em>Body</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Body</em>' containment reference.
   * @see #getBody()
   * @generated
   */
  void setBody(XExpression value);

} // Constructor
