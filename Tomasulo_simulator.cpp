#include<cstring>
#include<cmath>
#include<cstdio>
#include<iostream>
#include<fstream>
#include<utility>
#include<bitset>
using namespace std;
typedef long long ll;
typedef unsigned int ui;
#define test(x) cout<<(x);

class CPU
{
    public:
    ui reg[32],mem[50000],qreg[32];
    ui PC,IR,rd,rs1,rs2,imm;
    string rev;
    inline ui Hex_DC(char buf[],int st,int en)
    {
        ui x=0;
        for(int i=st;i<=en;++i)
        {
            if(buf[i]>='A'&&buf[i]<='F')
                x=(x<<4)+buf[i]-'A'+10;
            else
                x=(x<<4)+buf[i]-'0';
        }
        return x;
    }
    CPU(string filename):PC(0),IR(0),imm(0),rs1(0),rs2(0),rd(0),rev("IUSBRJE")
    {
        fstream f;
        f.open(filename,ios::in);
        if(!f.good())
        {
            printf("Open File Error");
            return;
        }
        char buffer[100];
        while(1)
        {
            f.getline(buffer,90);
            test(buffer);
            //test('\n');
            if(buffer[0]=='#')
                break;
            if(buffer[0]=='@')
            {
                PC=Hex_DC(buffer,1,8);
                //test(PC);
                //test('\n');
                continue;
            }
            
            int l=strlen(buffer);
            for(int i=0;i<l;i+=3)
            {    
                mem[PC++]=Hex_DC(buffer,i,i+1);
                //test(mem[PC-1]);
                //test(' ');
            }
            test('\n');
        }
        f.close();
        PC=0;
    }
    
    enum commandtype{
        I,U,S,B,R,UJ,ERR
    };
    enum command{
        LUI=0b0110111,
        AUIPC=0b0010111,
        JAL=0b1101111,
        JALR=0b1100111+(0b000<<7),
        BEQ=0b1100011+(0b000<<7),
        BNE=0b1100011+(0b001<<7),
        BLT=0b1100011+(0b100<<7),
        BGE=0b1100011+(0b101<<7),
        BLTU=0b1100011+(0b110<<7),
        BGEU=0b1100011+(0b111<<7),
        LB=0b0000011+(0b000<<7),
        LH=0b0000011+(0b001<<7),
        LW=0b0000011+(0b010<<7),
        LBU=0b0000011+(0b100<<7),
        LHU=0b0000011+(0b101<<7),
        SB=0b0100011+(0b000<<7),
        SH=0b0100011+(0b001<<7),
        SW=0b0100011+(0b010<<7),
        ADDI=0b0010011+(0b000<<7),
        SLTI=0b0010011+(0b010<<7),
        SLTIU=0b0010011+(0b011<<7),
        XORI=0b0010011+(0b100<<7),
        ORI=0b0010011+(0b110<<7),
        ANDI=0b0010011+(0b111<<7),
        SLLI=0b0010011+(0b001<<7)+(0b0000000<<10),
        SRLI=0b0010011+(0b101<<7)+(0b0000000<<10),
        SRAI=0b0010011+(0b101<<7)+(0b0100000<<10),
        ADD=0b0110011+(0b000<<7)+(0b0000000<<10),
        SUB=0b0110011+(0b000<<7)+(0b0100000<<10),
        SLL=0b0110011+(0b001<<7)+(0b0000000<<10),
        SLT=0b0110011+(0b010<<7)+(0b0000000<<10),
        SLTU=0b0110011+(0b011<<7)+(0b0000000<<10),
        XOR=0b0110011+(0b100<<7)+(0b0000000<<10),
        SRL=0b0110011+(0b101<<7)+(0b0000000<<10),
        SRA=0b0110011+(0b101<<7)+(0b0100000<<10),
        OR=0b0110011+(0b110<<7)+(0b0000000<<10),
        AND=0b0110011+(0b111<<7)+(0b0000000<<10),
        ERROR=-1
    };
    enum tomatype{
        FP,LOAD,STORE,NONE=-1
    };
    command cmdset[38]={LUI,AUIPC,JAL,JALR,BEQ,BNE,BLT,BGE,BLTU,BGEU,LB,LH,LW,LBU,LHU,SB,SH,SW,ADDI,SLTI,SLTIU,XORI,ORI,ANDI,SLLI,SRLI,ADD,SUB,SLL,SLT,SLTU,XOR,SRL,SRA,OR,AND};
    class input
    {
        public:
        ui x;
        command cmd;
        commandtype type;
        tomatype tomatype;
        ui A,vj,vk,qj,qk,dest;
        int time,tag;
        bool busy;
        bool solve;
        input(int t=0):x(t),type(commandtype::ERR),cmd(command::ERROR),tomatype(NONE){
            A=vj=vk=qj=qk=dest=time=busy=tag=solve=0;
        }
    };
    class queue
    {
        public:
        int rear,frt;
        input q[2000];
        int siz=2000;
        queue():rear(0),frt(0){}
        inline bool empty()
        {
            return rear==frt;
        }
        inline bool full()
        {
            return (rear+1)%siz==frt;
        }
        inline bool push(const input &x)
        {
            if(full())return false;
            q[++rear]=x;
            return true;
        }
        inline input front()
        {
            return q[(frt+1)%siz];
        }
        inline void pop()
        {
            if(empty());//MD?
            frt=(frt+1)%siz;
        }
    };
    queue ROB,IQ;
    inline commandtype get_type(command x)
    {
        switch (x) {
            case LUI:
            case AUIPC:
                return commandtype::U;
            case JAL:
                return commandtype::UJ;
            case JALR:
            case LB:
            case LH:
            case LW:
            case LBU:
            case LHU:
            case ADDI:
            case SLTI:
            case SLTIU:
            case XORI:
            case ORI:
            case ANDI:
            case SLLI:
            case SRLI:
            case SRAI:
                return commandtype::I;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                return commandtype::B;
            case SB:
            case SH:
            case SW:
                return commandtype::S;
            case ADD:
            case SUB:
            case SLL:
            case SLT:
            case SLTU:
            case XOR:
            case SRL:
            case SRA:
            case OR:
            case AND:
                return commandtype::R;
            default:
                std::cerr << "[Error]function [get_format()] wrong with a undefined CommandType." << std::endl;
                return commandtype::ERR;
        }
    }
    inline tomatype get_tomatype(command x)
    {
        switch (x)
        {
            case LB:
            case LH:
            case LW:
            case LBU:
            case LHU:
                return tomatype::LOAD;
            case SB:
            case SH:
            case SW:
                return tomatype::STORE;
            default:
                return tomatype::FP;
        }
    }
    inline ui get_opcode(ui x)
    {
        return (x&(ui)0x0000007f);
    }
    inline ui get_rs1(ui x)
    {
        return (x&(ui)0x000f8000)>>15;
    }
    inline ui get_rs2(ui x)
    {
        return (x&(ui)0x01f00000)>>20;
    }
    inline ui get_rd(ui x)
    {
        test("getrd\n");
        cout<<bitset<32>(x)<<endl;
        cout<<bitset<32>((x&((ui)0x00000f80))>>7)<<endl;
        return (x&((ui)0x00000f80))>>7;
    }
    inline ui get_funct3(ui x)
    {
        return (x&(ui)0x00007000)>>12;
    }
    inline ui get_funct7(ui x)
    {
        return (x&(ui)0x0fe000000)>>25;
    }
    inline ui get_shamt(ui x)
    {
        return (x&(ui)0x01f00000)>>20;
    }
    inline ui get_immediate(ui x,commandtype type)
    {
        int ans=0,tmp=0;
        switch(type)
        {
            case R:
                break;
            case I:
                ans = (x &(ui)0xfff00000) >> 20;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfffff000;
                break;
            case S:
                ans = (x & (ui) 0xfe000000) >> 20;
                tmp = (x & (ui) 0x00000f80) >> 7;
                ans |= tmp;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfffff000;
                break;
            case B:
                ans = (x & (ui) 0x7e000000) >> 20;
                tmp = (x & (ui) 0x00000f00) >> 7;
                ans |= tmp;
                tmp = (x & (ui) 0x00000080) << 4;
                ans |= tmp;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfffff000;
                break;
            case U:
                ans = x & (ui) 0xfffff000;
                break;
            case UJ:
                ans = (x & (ui) 0x7fe00000) >> 20;
                tmp = (x & (ui) 0x000ff000);
                ans |= tmp;
                tmp = (x & (ui) 0x00100000) >> 9;
                ans |= tmp;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfff00000;
                break;
            default:
                std::cerr << "[Error]function [get_immediate()] wrong with a undefined CommandType." << std::endl;
                break;
        }
        return ans;
    }

    inline void decode(input ip)
    {   
        //cout<<bitset<32>(ip.x)<<endl;
        ui code=get_opcode(ip.x),f3=get_funct3(ip.x),f7=get_funct7(ip.x);
        for(int i=0;i<37;++i)
            if(code&cmdset[i]==code)
            {
                commandtype tmp=get_type(cmdset[i]);
                if(tmp==U||tmp==UJ)
                {   
                    ip.cmd=cmdset[i];
                    break;
                }
                if(f3&cmdset[i]!=f3)
                    continue;
                if(tmp==R&&f7&cmdset[i]!=f7)
                    continue;
                ip.cmd=cmdset[i];
                break;
            }
        ip.type=get_type(ip.cmd);
        ip.tomatype=get_tomatype(ip.cmd);
        switch (ip.tomatype)
        {
            case LOAD:
            case STORE:
                ip.time=3;
                break;
            default:
                ip.time=1;
        }
        switch (ip.type)
        {
            case R:
                rd=get_rd(ip.x);
                rs1=get_rs1(ip.x);
                rs2=get_rs2(ip.x);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                qreg[rd]=(IQ.rear+1)%IQ.siz;
                ip.dest=rd;
                break;
            case I:
                rd=get_rd(ip.x);
                rs1=get_rs1(ip.x);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                ip.dest=rd;
                qreg[rd]=(IQ.rear+1)%IQ.siz;
                break;
            case S:
            case B:
                rs1=get_rs1(ip.x);
                rs2=get_rs2(ip.x);
                imm=get_immediate(ip.x,ip.type);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                ip.A=imm;
                break;
            case U:
            case UJ:
                rd=get_rd(ip.x);
                imm=get_immediate(ip.x,ip.type);
                ip.A=imm;
                ip.dest=rd;
                qreg[rd]=(IQ.rear+1)%IQ.siz;
                break;
            default:
                cerr<<"Error at [decoding]: dicover unexpected commandtype"<<endl; 
                break;
        }
        ROB.push(ip);
        ip.tag=ROB.rear;
        IQ.push(ip);
        ROB.q[ROB.rear].tag=IQ.rear;
        /*printf("PC:%u opcode:%u type:%c funct3:%u funt7:%u rs1:%u rs2:%u rd:%u imm:%u\n",PC,code,rev[ip.type],f3,f7,rs1,rs2,rd,imm);
        fstream f("output.data",ios::out|ios::app);
        f<<code<<" ";
        f.close();*/

    }
    inline void sign_extend(ui &x,int pos){
        x|=(((ui)0xfffffff)>>pos)<<pos;
    }
    inline void IF()
    {
        ui x=0;
        for(int i=3;i>=0;--i)
            x=(x<<8)|mem[PC+i];
        PC+=4;
        decode(input(x));
    }
    inline ui EXE(input ip)//MD?
    {
        ui ans;
        switch (ip.cmd)
        {
            case LUI:
            case AUIPC:
            case JAL:
            case JALR:
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                break;
            case LB:
                ip.A=ip.qj+(int)imm;
                break;
            case LH:
                ip.A=ip.qj+(int)imm;
                break;    
            case LW:
                ip.A=ip.qj+(int)imm;
                break; 
            case LBU:
                ip.A=ip.qj+(int)imm;
                break;
            case LHU:
                ip.A=ip.qj+(int)imm;
                break;
            case SB:
                ip.A=ip.qj+(int)imm;
                break;
            case SH:
                ip.A=ip.qj+(int)imm;
                break;
            case SW:
                ip.A=ip.qj+(int)imm;
                break;                                                                 
            case ADDI:
                reg[rd]=reg[rs1]+(int)imm;
                break;
            case SLTI:
                reg[rd]=((int)reg[rs1]<(int)imm)?1:0;
                break;
            case SLTIU:
                reg[rd]=(reg[rs1]<imm)?1:0;
                break;
            case XORI:
                reg[rd]=reg[rs1]^imm;
                break;
            case ORI:
                reg[rd]=reg[rs1]|imm;
                break;
            case ANDI:
                reg[rd]=reg[rs1]&imm;
                break;
            case SLLI:
                reg[rd]=reg[rs1]<<imm;
                break;
            case SRLI:
                reg[rd]=reg[rs1]>>imm;
                break;
            case SRAI://?????
                reg[rd]=reg[rs1]>>imm;
                break;
            case ADD:
                reg[rd]=reg[rs1]+reg[rs2];
                break;
            case SUB:
                reg[rd]=reg[rs1]-reg[rs2];
                break;
            case SLL:
                reg[rd]=reg[rs1]<<reg[rs2];
                break;
            case SLT:
                reg[rd]=((int)reg[rs1]<(int)reg[rs2])?1:0;
                break;
            case SLTU:
                reg[rd]=(reg[rs1]<reg[rs2])?1:0;
                break;
            case XOR:
                reg[rd]=reg[rs1]^reg[rs2];
                break;
            case SRL:
                reg[rd]=reg[rs1]>>reg[rs2];
                break;
            case SRA:
                reg[rd]=reg[rs1]>>reg[rs2];
                break;
            case OR:
                reg[rd]=reg[rs1]|reg[rs2];
                break;
            case AND:
                reg[rd]=reg[rs1]&reg[rs2];
                break;
            default:
                cerr<<"unexpected command at [EXE]"<<endl;
        }
        return ans;
    }
    inline ui WB(input ip)//MD?
    {
        ui ans;
        switch (ip.cmd)
        {
            case LUI:
                reg[rd]=(imm);
                break;
            case AUIPC:
                mem[PC]=imm;
                reg[rd]=mem[PC];
            case JAL:
                reg[rd]=PC+4;
                PC+=imm;
                break;
            case JALR:
                reg[rd]=PC+4;
                PC+=imm;
                break;
            case BEQ:
                if(reg[rs1]==reg[rs2])
                    PC+=imm;
                break;
            case BNE:
                if(reg[rs1]!=reg[rs2])
                    PC=reg[rs1]+imm;
                break;
            case BLT:
                if((int)reg[rs1]<(int)reg[rs2])
                    PC+=imm;
                break;
            case BGE:
                if((int)reg[rs1]>=(int)reg[rs2])
                    PC+=imm;
                break;
            case BLTU:
                if(reg[rs1]<reg[rs2])
                    PC+=imm;
                break;
            case BGEU:
                if(reg[rs1]>=reg[rs2])
                    PC+=imm;
                break;
            case LB:
                ans=mem[reg[rs1]+(int)imm];
                sign_extend(ans,7);
                reg[rd]=ans;
                break;
            case LH:
                ans=mem[reg[rs1]+(int)imm];
                ans=(ans<<8)|mem[reg[rs1]+(int)imm+1];
                sign_extend(ans,15);
                reg[rd]=ans;
                break;    
            case LW:
                ans=0;
                for(int i=reg[rs1]+(int)imm;i<reg[rs1]+(int)imm+4;++i)
                    ans=(ans<<8)|mem[i];
                reg[rd]=ans;
                break;
            case LBU:
                ans=mem[reg[rs1]+(int)imm];
                reg[rd]=ans;
                break;
            case LHU:
                ans=mem[reg[rs1]+(int)imm];
                ans=(ans<<8)|mem[reg[rs1]+(int)imm+1];
                reg[rd]=ans;
                break;
            case SB:
                ans=reg[rs2]&((ui)0xff);
                mem[reg[rs1]+(int)imm+3]=((reg[rs2]>>8)<<8)|ans;
                break;
            case SH:
                ans=reg[rs2]&((ui)0xffff);
                mem[reg[rs1]+(int)imm+3]=ans&((ui)0xff);
                mem[reg[rs1]+(int)imm+2]=(ans&(ui)0xff00)>>8;
                break;
            case SW:
                for(int i=3;i>=0;--i)
                    mem[reg[rs1]+(int)imm+i]=(reg[rs2]&((ui)0xff<<(3-i)))>>(3-i);
                break;
            case ADDI:
                reg[rd]=reg[rs1]+(int)imm;
                break;
            case SLTI:
                reg[rd]=((int)reg[rs1]<(int)imm)?1:0;
                break;
            case SLTIU:
                reg[rd]=(reg[rs1]<imm)?1:0;
                break;
            case XORI:
                reg[rd]=reg[rs1]^imm;
                break;
            case ORI:
                reg[rd]=reg[rs1]|imm;
                break;
            case ANDI:
                reg[rd]=reg[rs1]&imm;
                break;
            case SLLI:
                reg[rd]=reg[rs1]<<imm;
                break;
            case SRLI:
                reg[rd]=reg[rs1]>>imm;
                break;
            case SRAI://?????
                reg[rd]=reg[rs1]>>imm;
                break;
            case ADD:
                reg[rd]=reg[rs1]+reg[rs2];
                break;
            case SUB:
                reg[rd]=reg[rs1]-reg[rs2];
                break;
            case SLL:
                reg[rd]=reg[rs1]<<reg[rs2];
                break;
            case SLT:
                reg[rd]=((int)reg[rs1]<(int)reg[rs2])?1:0;
                break;
            case SLTU:
                reg[rd]=(reg[rs1]<reg[rs2])?1:0;
                break;
            case XOR:
                reg[rd]=reg[rs1]^reg[rs2];
                break;
            case SRL:
                reg[rd]=reg[rs1]>>reg[rs2];
                break;
            case SRA:
                reg[rd]=reg[rs1]>>reg[rs2];
                break;
            case OR:
                reg[rd]=reg[rs1]|reg[rs2];
                break;
            case AND:
                reg[rd]=reg[rs1]&reg[rs2];
                break;
            default:
                cerr<<"unexpected command at [EXE]"<<endl;
        }
        return ans;       
    }
    inline ui COMMIT(input ip)//MD?
    {}
    inline void upd()
    {
        for(int i=(IQ.frt+1)%IQ.siz;i!=IQ.rear;i=(i+1)%IQ.siz)
        {
            if(IQ.q[i].time==-1)
                continue;
            if(!IQ.q[i].time)
            {
                WB(IQ.q[i]);
                if(IQ.q[i].tomatype!=STORE)
                {
                    for(int j=(IQ.frt+1)%IQ.siz;j!=IQ.rear;j=(j+1)%IQ.siz)
                    {
                        if(IQ.q[j].qk==i)
                            IQ.q[j].qk=0;
                        if(IQ.q[j].qj==i)
                            IQ.q[j].qj=0;
                    }
                    for(int i=0;i<32;++i)
                        if(qreg[i]==i)
                            qreg[i]=0;
                    ROB.q[IQ.q[i].tag].busy=true;
                    ROB.q[IQ.q[i].tag].A=IQ.q[i].A;
                }
                --IQ.q[i].time;
            }
            if((!IQ.q[i].qj)&(!IQ.q[i].qk))
            {
                if((IQ.q[i].tomatype==LOAD||IQ.q[i].tomatype==STORE)&&(!IQ.q[i].solve))
                {
                    EXE(IQ.q[i]);
                    IQ.q[i].solve=1;
                    ROB.q[IQ.q[i].tag].A=IQ.q[i].A;
                    for(int j=(ROB.frt+1)%ROB.siz;j!=IQ.q[i].tag;j=(j+1)%ROB.siz)
                    {
                        if(IQ.q[j].tomatype==STORE&&ROB.q[j].A==IQ.q[i].A)
                        {
                            IQ.q[i].qj=ROB.q[j].tag;
                            break;
                        }
                    }
                    if(!IQ.q[i].qj)
                        IQ.q[i].busy=true;
                    continue;
                }
            if(IQ.q[i].busy)
                --IQ.q[i].time;
            }
        }
        if(ROB.front().busy)
        {
            COMMIT(ROB.front());
            if(ROB.front().tomatype==STORE)
            {
                for(int j=(IQ.frt+1)%IQ.siz;j!=IQ.rear;j=(j+1)%IQ.siz)
                {
                    if(IQ.q[j].qj==ROB.front().tag)
                        IQ.q[j].qj=0;
                }
            }
            ROB.pop();
            IQ.pop();//MD?
        }
    }
    inline void print()
    {
        for(int i=0;i<20;++i)
            printf("%d ",mem[i]);
        return;
    }
};

int main()
{
    fstream f("output.data",ios::out|ios::trunc);
    f.close();
    CPU T("array_test1.data");
    for(int i=1;i<=5;++i)
    {
        T.IF();
        T.upd();
    }
    return 0;
}