#include <math.h>

#define NONE 0
#define ADD 1
#define MULT 2
#define DAMPED 1
#define TOL 1.0e-10
#define HUGEN 1.0e10
#define NA  -99999.0

// Functions called by R
void etscalc(double *, int *, double *, int *, int *, int *, int *,
    double *, double *, double *, double *, double *, double *, double *, int*);
void etssimulate(double *, int *, int *, int *, int *,
    double *, double *, double *, double *, int *, double *, double *);
void etsforecast(double *, int *, int *, int *, double *, int *, double *);

// Internal functions
void forecast(double, double, double *, int, int, int, double, double *, int);
void update(double *, double *, double *, double *, double *, double *, int, int, int,
    double, double, double, double, double);

// ******************************************************************

void etscalc(double *y, int *n, double *x, int *m, int *error, int *trend, int *season,
    double *alpha, double *beta, double *gamma, double *phi, double *e, double *lik, double *amse, int *nmse)
{
    int i, j, nstates;
    double oldl, l, oldb, b, olds[24], s[24], f[30], lik2, tmp, denom[30];

    if((*m > 24) & (*season > NONE))
        return;
    else if(*m < 1)
        *m = 1;

    if(*nmse > 30)
        *nmse = 30;

    nstates = (*m)*(*season>NONE) + 1 + (*trend>NONE);

    // Copy initial state components
    l = x[0];
    if(*trend > NONE)
        b = x[1];
    if(*season > NONE)
    {
        for(j=0; j<(*m); j++)
            s[j] = x[(*trend>NONE)+j+1];
    }

    *lik = 0.0;
    lik2 = 0.0;
    for(j=0; j<(*nmse); j++)
    {
        amse[j] = 0.0;
        denom[j] = 0.0;
    }

    for (i=0; i<(*n); i++)
    {
        // COPY PREVIOUS STATE
        oldl = l;
        if(*trend > NONE)
            oldb = b;
        if(*season > NONE)
        {
            for(j=0; j<(*m); j++)
                olds[j] = s[j];
        }

        // ONE STEP FORECAST
        forecast(oldl, oldb, olds, *m, *trend, *season, *phi, f, *nmse);
        if(fabs(f[0]-NA) < TOL)
        {
            *lik = NA;
            return;
        }

        if(*error == ADD)
            e[i] = y[i] - f[0];
        else
            e[i] = (y[i] - f[0])/f[0];
        for(j=0; j<(*nmse); j++)
        {
            if(i+j<(*n))
            {
                denom[j] += 1.0;
                tmp = y[i+j]-f[j];
                amse[j] = (amse[j] * (denom[j]-1.0) + (tmp*tmp)) / denom[j];
            }
        }

        // UPDATE STATE
        update(&oldl, &l, &oldb, &b, olds, s, *m, *trend, *season, *alpha, *beta, *gamma, *phi, y[i]);

        // STORE NEW STATE
        x[nstates*(i+1)] = l;
        if(*trend > NONE)
            x[nstates*(i+1)+1] = b;
        if(*season > NONE)
        {
           for(j=0; j<(*m); j++)
                x[(*trend>NONE)+nstates*(i+1)+j+1] = s[j];
        }
        *lik = *lik + e[i]*e[i];
        lik2 += log(fabs(f[0]));
    }
    *lik = (*n) * log(*lik);
    if(*error == MULT)
        *lik += 2*lik2;
}

// *********************************************************************************

void etssimulate(double *x, int *m, int *error, int *trend, int *season,
    double *alpha, double *beta, double *gamma, double *phi, int *h, double *y, double *e)

{
    int i, j, nstates;
    double oldl, l, oldb, b, olds[24], s[24], f[10];

    if((*m > 24) & (*season > NONE))
        return;
	else if(*m < 1)
		*m = 1;

    nstates = (*m)*(*season>NONE) + 1 + (*trend>NONE);

    // Copy initial state components
    l = x[0];
    if(*trend > NONE)
        b = x[1];
    if(*season > NONE)
    {
        for(j=0; j<(*m); j++)
            s[j] = x[(*trend>NONE)+j+1];
    }

    for (i=0; i<(*h); i++)
    {
        // COPY PREVIOUS STATE
        oldl = l;
        if(*trend > NONE)
            oldb = b;
        if(*season > NONE)
        {
            for(j=0; j<(*m); j++)
                olds[j] = s[j];
        }

        // ONE STEP FORECAST
        forecast(oldl, oldb, olds, *m, *trend, *season, *phi, f, 1);
        if(fabs(f[0]-NA) < TOL)
        {
            y[0]=NA;
            return;
        }

        if(*error == ADD)
            y[i] = f[0] + e[i];
        else
            y[i] = f[0]*(1.0+e[i]);

        // UPDATE STATE
        update(&oldl, &l, &oldb, &b, olds, s, *m, *trend, *season, *alpha, *beta, *gamma, *phi, y[i]);
    }
}

// *********************************************************************************

void etsforecast(double *x, int *m, int *trend, int *season, double *phi, int *h, double *f)

{
    int j;
    double l, b, s[24];

    if((*m > 24) & (*season > NONE))
        return;
    else if(*m < 1)
        *m = 1;

    // Copy initial state components
    l = x[0];
	b = 0.0;
    if(*trend > NONE)
        b = x[1];
    if(*season > NONE)
    {
        for(j=0; j<(*m); j++)
            s[j] = x[(*trend>NONE)+j+1];
    }

    // Compute forecasts
    forecast(l, b, s, *m, *trend, *season, *phi, f, *h);
}

// *****************************************************************

void forecast(double l, double b, double *s, int m, int trend, int season, double phi, double *f, int h)
{
    int i,j;
    double phistar;

    phistar = phi;

    // FORECASTS
    for(i=0; i<h; i++)
    {
        if(trend == NONE)
            f[i] = l;
        else if(trend == ADD)
            f[i] = l + phistar*b;
        else if(b<0)
            f[i] = NA;
        else
            f[i] = l * pow(b,phistar);
        j = m-1-i;
        while(j < 0)
             j += m;
        if(season == ADD)
            f[i] = f[i] + s[j];
        else if(season == MULT)
            f[i] = f[i] * s[j];
        if(i < (h-1))
        {
            if(fabs(phi-1.0) < TOL)
                phistar = phistar + 1.0;
            else
                phistar = phistar + pow(phi, (double) (i+1));
        }
    }
}

// *****************************************************************

void update(double *oldl, double *l, double *oldb, double *b, double *olds, double *s, int m, int trend, int season,
    double alpha, double beta, double gamma, double phi, double y)
{
    int j;
    double q, p, r, t, phib;

    // NEW LEVEL
    if(trend==NONE)
	{
        q = *oldl;                 // l(t-1)
		phib = 0;
	}
    else if(trend==ADD)
    {
        phib = phi*(*oldb);
        q = *oldl + phib;          // l(t-1) + phi*b(t-1)
    }
    else if(fabs(phi-1.0) < TOL)
    {
        phib = *oldb;
        q = *oldl * (*oldb);       // l(t-1)*b(t-1)
    }
    else
    {
        phib = pow(*oldb,phi);
        q = (*oldl) * phib;          // l(t-1)*b(t-1)^phi
    }
    if(season==NONE)
        p = y;
    else if(season==ADD)
        p = y - olds[m-1];         // y[t] - s[t-m]
    else
    {
        if(fabs(olds[m-1]) < TOL)
            p = HUGEN;
        else
            p = y / olds[m-1];     // y[t]/s[t-m]
    }
    *l = q + alpha*(p-q);

    // NEW GROWTH
    if(trend > NONE)
    {
        if(trend==ADD)
           r = (*l) - (*oldl);       // l[t]-l[t-1]
        else //if(trend==MULT)
        {
            if(fabs(*oldl) < TOL)
                r = HUGEN;
            else
                r = (*l)/(*oldl);    // l[t]/l[t-1]
        }
        *b = phib + (beta/alpha)*(r - phib);   // b[t] = phi*b[t-1] + beta*(r - phi*b[t-1])
                                               // b[t] = b[t-1]^phi + beta*(r - b[t-1]^phi)
    }

    // NEW SEASON
    if(season > NONE)
    {
        if(season==ADD)
            t = y - q;
        else //if(season==MULT)
        {
            if(fabs(q) < TOL)
                t = HUGEN;
            else
                t = y / q;
        }
        s[0] = olds[m-1] + gamma*(t - olds[m-1]); // s[t] = s[t-m] + gamma*(t - s[t-m])
        for(j=1; j<m; j++)
            s[j] = olds[j-1];                     // s[t] = s[t]
    }
}
