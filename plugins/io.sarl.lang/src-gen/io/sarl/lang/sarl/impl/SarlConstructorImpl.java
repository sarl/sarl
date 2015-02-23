/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.XtendParameter;

import org.eclipse.xtend.core.xtend.impl.XtendMemberImpl;

import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Constructor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlConstructorImpl#getTypeParameters <em>Type Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlConstructorImpl#getParameters <em>Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlConstructorImpl#isVarargs <em>Varargs</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlConstructorImpl#getExceptions <em>Exceptions</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlConstructorImpl#getExpression <em>Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SarlConstructorImpl extends XtendMemberImpl implements SarlConstructor
{
  /**
   * The cached value of the '{@link #getTypeParameters() <em>Type Parameters</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTypeParameters()
   * @generated
   * @ordered
   */
  protected EList<JvmTypeParameter> typeParameters;

  /**
   * The cached value of the '{@link #getParameters() <em>Parameters</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getParameters()
   * @generated
   * @ordered
   */
  protected EList<XtendParameter> parameters;

  /**
   * The default value of the '{@link #isVarargs() <em>Varargs</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isVarargs()
   * @generated
   * @ordered
   */
  protected static final boolean VARARGS_EDEFAULT = false;

  /**
   * The cached value of the '{@link #isVarargs() <em>Varargs</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isVarargs()
   * @generated
   * @ordered
   */
  protected boolean varargs = VARARGS_EDEFAULT;

  /**
   * The cached value of the '{@link #getExceptions() <em>Exceptions</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExceptions()
   * @generated
   * @ordered
   */
  protected EList<JvmTypeReference> exceptions;

  /**
   * The cached value of the '{@link #getExpression() <em>Expression</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExpression()
   * @generated
   * @ordered
   */
  protected XExpression expression;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SarlConstructorImpl()
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
    return SarlPackage.Literals.SARL_CONSTRUCTOR;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmTypeParameter> getTypeParameters()
  {
    if (typeParameters == null)
    {
      typeParameters = new EObjectContainmentEList<JvmTypeParameter>(JvmTypeParameter.class, this, SarlPackage.SARL_CONSTRUCTOR__TYPE_PARAMETERS);
    }
    return typeParameters;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<XtendParameter> getParameters()
  {
    if (parameters == null)
    {
      parameters = new EObjectContainmentEList<XtendParameter>(XtendParameter.class, this, SarlPackage.SARL_CONSTRUCTOR__PARAMETERS);
    }
    return parameters;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean isVarargs()
  {
    return varargs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVarargs(boolean newVarargs)
  {
    boolean oldVarargs = varargs;
    varargs = newVarargs;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_CONSTRUCTOR__VARARGS, oldVarargs, varargs));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmTypeReference> getExceptions()
  {
    if (exceptions == null)
    {
      exceptions = new EObjectContainmentEList<JvmTypeReference>(JvmTypeReference.class, this, SarlPackage.SARL_CONSTRUCTOR__EXCEPTIONS);
    }
    return exceptions;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public XExpression getExpression()
  {
    return expression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetExpression(XExpression newExpression, NotificationChain msgs)
  {
    XExpression oldExpression = expression;
    expression = newExpression;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_CONSTRUCTOR__EXPRESSION, oldExpression, newExpression);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setExpression(XExpression newExpression)
  {
    if (newExpression != expression)
    {
      NotificationChain msgs = null;
      if (expression != null)
        msgs = ((InternalEObject)expression).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_CONSTRUCTOR__EXPRESSION, null, msgs);
      if (newExpression != null)
        msgs = ((InternalEObject)newExpression).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_CONSTRUCTOR__EXPRESSION, null, msgs);
      msgs = basicSetExpression(newExpression, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_CONSTRUCTOR__EXPRESSION, newExpression, newExpression));
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
      case SarlPackage.SARL_CONSTRUCTOR__TYPE_PARAMETERS:
        return ((InternalEList<?>)getTypeParameters()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_CONSTRUCTOR__PARAMETERS:
        return ((InternalEList<?>)getParameters()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_CONSTRUCTOR__EXCEPTIONS:
        return ((InternalEList<?>)getExceptions()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_CONSTRUCTOR__EXPRESSION:
        return basicSetExpression(null, msgs);
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
      case SarlPackage.SARL_CONSTRUCTOR__TYPE_PARAMETERS:
        return getTypeParameters();
      case SarlPackage.SARL_CONSTRUCTOR__PARAMETERS:
        return getParameters();
      case SarlPackage.SARL_CONSTRUCTOR__VARARGS:
        return isVarargs();
      case SarlPackage.SARL_CONSTRUCTOR__EXCEPTIONS:
        return getExceptions();
      case SarlPackage.SARL_CONSTRUCTOR__EXPRESSION:
        return getExpression();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_CONSTRUCTOR__TYPE_PARAMETERS:
        getTypeParameters().clear();
        getTypeParameters().addAll((Collection<? extends JvmTypeParameter>)newValue);
        return;
      case SarlPackage.SARL_CONSTRUCTOR__PARAMETERS:
        getParameters().clear();
        getParameters().addAll((Collection<? extends XtendParameter>)newValue);
        return;
      case SarlPackage.SARL_CONSTRUCTOR__VARARGS:
        setVarargs((Boolean)newValue);
        return;
      case SarlPackage.SARL_CONSTRUCTOR__EXCEPTIONS:
        getExceptions().clear();
        getExceptions().addAll((Collection<? extends JvmTypeReference>)newValue);
        return;
      case SarlPackage.SARL_CONSTRUCTOR__EXPRESSION:
        setExpression((XExpression)newValue);
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
      case SarlPackage.SARL_CONSTRUCTOR__TYPE_PARAMETERS:
        getTypeParameters().clear();
        return;
      case SarlPackage.SARL_CONSTRUCTOR__PARAMETERS:
        getParameters().clear();
        return;
      case SarlPackage.SARL_CONSTRUCTOR__VARARGS:
        setVarargs(VARARGS_EDEFAULT);
        return;
      case SarlPackage.SARL_CONSTRUCTOR__EXCEPTIONS:
        getExceptions().clear();
        return;
      case SarlPackage.SARL_CONSTRUCTOR__EXPRESSION:
        setExpression((XExpression)null);
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
      case SarlPackage.SARL_CONSTRUCTOR__TYPE_PARAMETERS:
        return typeParameters != null && !typeParameters.isEmpty();
      case SarlPackage.SARL_CONSTRUCTOR__PARAMETERS:
        return parameters != null && !parameters.isEmpty();
      case SarlPackage.SARL_CONSTRUCTOR__VARARGS:
        return varargs != VARARGS_EDEFAULT;
      case SarlPackage.SARL_CONSTRUCTOR__EXCEPTIONS:
        return exceptions != null && !exceptions.isEmpty();
      case SarlPackage.SARL_CONSTRUCTOR__EXPRESSION:
        return expression != null;
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
    result.append(" (varargs: ");
    result.append(varargs);
    result.append(')');
    return result.toString();
  }

} //SarlConstructorImpl
