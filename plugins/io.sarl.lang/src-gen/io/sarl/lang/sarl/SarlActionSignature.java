/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.CreateExtensionInfo;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;

import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Action Signature</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getTypeParameters <em>Type Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getParameters <em>Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#isVarargs <em>Varargs</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getReturnType <em>Return Type</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getCreateExtensionInfo <em>Create Extension Info</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getExceptions <em>Exceptions</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlActionSignature#getFiredEvents <em>Fired Events</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature()
 * @model
 * @generated
 */
public interface SarlActionSignature extends XtendMember
{
  /**
   * Returns the value of the '<em><b>Type Parameters</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmTypeParameter}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Type Parameters</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Type Parameters</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_TypeParameters()
   * @model containment="true"
   * @generated
   */
  EList<JvmTypeParameter> getTypeParameters();

  /**
   * Returns the value of the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' attribute.
   * @see #setName(String)
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.SarlActionSignature#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

  /**
   * Returns the value of the '<em><b>Parameters</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtend.core.xtend.XtendParameter}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Parameters</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Parameters</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_Parameters()
   * @model containment="true"
   * @generated
   */
  EList<XtendParameter> getParameters();

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
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_Varargs()
   * @model
   * @generated
   */
  boolean isVarargs();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.SarlActionSignature#isVarargs <em>Varargs</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Varargs</em>' attribute.
   * @see #isVarargs()
   * @generated
   */
  void setVarargs(boolean value);

  /**
   * Returns the value of the '<em><b>Return Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Return Type</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Return Type</em>' containment reference.
   * @see #setReturnType(JvmTypeReference)
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_ReturnType()
   * @model containment="true"
   * @generated
   */
  JvmTypeReference getReturnType();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.SarlActionSignature#getReturnType <em>Return Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Return Type</em>' containment reference.
   * @see #getReturnType()
   * @generated
   */
  void setReturnType(JvmTypeReference value);

  /**
   * Returns the value of the '<em><b>Create Extension Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Create Extension Info</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Create Extension Info</em>' containment reference.
   * @see #setCreateExtensionInfo(CreateExtensionInfo)
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_CreateExtensionInfo()
   * @model containment="true"
   * @generated
   */
  CreateExtensionInfo getCreateExtensionInfo();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.SarlActionSignature#getCreateExtensionInfo <em>Create Extension Info</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Create Extension Info</em>' containment reference.
   * @see #getCreateExtensionInfo()
   * @generated
   */
  void setCreateExtensionInfo(CreateExtensionInfo value);

  /**
   * Returns the value of the '<em><b>Exceptions</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Exceptions</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Exceptions</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_Exceptions()
   * @model containment="true"
   * @generated
   */
  EList<JvmTypeReference> getExceptions();

  /**
   * Returns the value of the '<em><b>Fired Events</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Fired Events</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Fired Events</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlActionSignature_FiredEvents()
   * @model containment="true"
   * @generated
   */
  EList<JvmTypeReference> getFiredEvents();

} // SarlActionSignature
