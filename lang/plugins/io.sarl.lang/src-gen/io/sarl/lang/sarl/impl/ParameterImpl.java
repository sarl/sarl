/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.Parameter;
import io.sarl.lang.sarl.SarlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Parameter</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.ParameterImpl#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.ParameterImpl#getParameterType <em>Parameter Type</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.ParameterImpl#isVarArg <em>Var Arg</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ParameterImpl extends MinimalEObjectImpl.Container implements Parameter
{
  /**
   * The default value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected static final String NAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected String name = NAME_EDEFAULT;

  /**
   * The cached value of the '{@link #getParameterType() <em>Parameter Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getParameterType()
   * @generated
   * @ordered
   */
  protected JvmTypeReference parameterType;

  /**
   * The default value of the '{@link #isVarArg() <em>Var Arg</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isVarArg()
   * @generated
   * @ordered
   */
  protected static final boolean VAR_ARG_EDEFAULT = false;

  /**
   * The cached value of the '{@link #isVarArg() <em>Var Arg</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isVarArg()
   * @generated
   * @ordered
   */
  protected boolean varArg = VAR_ARG_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ParameterImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SarlPackage.Literals.PARAMETER;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getName()
  {
    return name;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setName(String newName)
  {
    String oldName = name;
    name = newName;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.PARAMETER__NAME, oldName, name));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public JvmTypeReference getParameterType()
  {
    return parameterType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetParameterType(JvmTypeReference newParameterType, NotificationChain msgs)
  {
    JvmTypeReference oldParameterType = parameterType;
    parameterType = newParameterType;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.PARAMETER__PARAMETER_TYPE, oldParameterType, newParameterType);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setParameterType(JvmTypeReference newParameterType)
  {
    if (newParameterType != parameterType)
    {
      NotificationChain msgs = null;
      if (parameterType != null)
        msgs = ((InternalEObject)parameterType).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.PARAMETER__PARAMETER_TYPE, null, msgs);
      if (newParameterType != null)
        msgs = ((InternalEObject)newParameterType).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.PARAMETER__PARAMETER_TYPE, null, msgs);
      msgs = basicSetParameterType(newParameterType, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.PARAMETER__PARAMETER_TYPE, newParameterType, newParameterType));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean isVarArg()
  {
    return varArg;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVarArg(boolean newVarArg)
  {
    boolean oldVarArg = varArg;
    varArg = newVarArg;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.PARAMETER__VAR_ARG, oldVarArg, varArg));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SarlPackage.PARAMETER__PARAMETER_TYPE:
        return basicSetParameterType(null, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SarlPackage.PARAMETER__NAME:
        return getName();
      case SarlPackage.PARAMETER__PARAMETER_TYPE:
        return getParameterType();
      case SarlPackage.PARAMETER__VAR_ARG:
        return isVarArg();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.PARAMETER__NAME:
        setName((String)newValue);
        return;
      case SarlPackage.PARAMETER__PARAMETER_TYPE:
        setParameterType((JvmTypeReference)newValue);
        return;
      case SarlPackage.PARAMETER__VAR_ARG:
        setVarArg((Boolean)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.PARAMETER__NAME:
        setName(NAME_EDEFAULT);
        return;
      case SarlPackage.PARAMETER__PARAMETER_TYPE:
        setParameterType((JvmTypeReference)null);
        return;
      case SarlPackage.PARAMETER__VAR_ARG:
        setVarArg(VAR_ARG_EDEFAULT);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.PARAMETER__NAME:
        return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
      case SarlPackage.PARAMETER__PARAMETER_TYPE:
        return parameterType != null;
      case SarlPackage.PARAMETER__VAR_ARG:
        return varArg != VAR_ARG_EDEFAULT;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (name: ");
    result.append(name);
    result.append(", varArg: ");
    result.append(varArg);
    result.append(')');
    return result.toString();
  }

} //ParameterImpl
